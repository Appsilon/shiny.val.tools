test_that("plan_artifact_paths returns slug-based doc/html/inventory paths", {
  records <- list(
    list(name = "doubled", kind = "feature"),
    list(name = "app/view/mod_card", kind = "module")
  )
  inv_features <- list(
    "doubled" = list(),
    "app/view/mod_card" = list()
  )
  paths <- plan_artifact_paths(records, inv_features)

  expect_true("doubled.md" %in% paths)
  expect_true("doubled.html" %in% paths)
  expect_true("doubled/inventory.json" %in% paths)
  expect_true("app--view--mod_card.md" %in% paths)
  expect_true("app--view--mod_card.html" %in% paths)
  expect_true("app--view--mod_card/inventory.json" %in% paths)
})

test_that("plan_artifact_paths skips inventory.json when no inventory record exists", {
  records <- list(list(name = "x", kind = "feature"))
  paths <- plan_artifact_paths(records, inventory_features = NULL)
  expect_setequal(paths, c("x.md", "x.html", "index.md", "index.html",
                           "validation-report.md"))
})

test_that("plan_artifact_paths always includes the index pair and the report", {
  paths <- plan_artifact_paths(list(), inventory_features = NULL)
  expect_setequal(paths, c("index.md", "index.html", "validation-report.md"))
})

test_that("svt_render writes a manifest with MD5 entries for every artifact", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  result <- svt_render(feats, inv, out_dir = out_dir)

  expect_true(file.exists(result$manifest))
  parsed <- yaml::yaml.load(paste(readLines(result$manifest), collapse = "\n"))
  expect_equal(parsed$schema_version, "1.0")
  expect_true(length(parsed$artifacts) >= 1L)
  for (art in parsed$artifacts) {
    expect_true(nchar(art$md5) == 32L)
    expect_true(file.exists(file.path(out_dir, art$path)))
  }
})

test_that("svt_render aborts when out_dir is non-empty without a manifest", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  writeLines("user file", file.path(out_dir, "notes.md"))

  expect_error(svt_render(feats, inv, out_dir = out_dir),
               "non-empty.*svt_manifest")
})

test_that("svt_render re-run deletes pristine orphan from prior manifest", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  # Synthesize an orphan: write a file we did not produce, then patch
  # the manifest to claim it as ours so the next run sees it as a
  # pristine orphan to delete.
  ghost_md <- "ghost.md"
  ghost_html <- "ghost.html"
  writeLines("auto", file.path(out_dir, ghost_md))
  writeLines("auto", file.path(out_dir, ghost_html))
  ghost_md_md5 <- unname(tools::md5sum(file.path(out_dir, ghost_md)))
  ghost_html_md5 <- unname(tools::md5sum(file.path(out_dir, ghost_html)))

  m_path <- file.path(out_dir, ".svt_manifest.json")
  m <- yaml::yaml.load(paste(readLines(m_path), collapse = "\n"))
  m$artifacts[[length(m$artifacts) + 1L]] <- list(path = ghost_md,
                                                   md5 = ghost_md_md5)
  m$artifacts[[length(m$artifacts) + 1L]] <- list(path = ghost_html,
                                                   md5 = ghost_html_md5)
  arts <- vapply(m$artifacts, function(a) {
    paste0("{\"path\":\"", a$path, "\",\"md5\":\"", a$md5, "\"}")
  }, character(1))
  body <- paste0("{\"schema_version\":\"1.0\",\"artifacts\":[",
                 paste(arts, collapse = ","), "]}")
  writeLines(body, m_path)

  svt_render(feats, inv, out_dir = out_dir)
  expect_false(file.exists(file.path(out_dir, ghost_md)))
  expect_false(file.exists(file.path(out_dir, ghost_html)))
})

test_that("svt_render aborts when an orphan has been edited", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  # Inject a fake "we wrote this" entry into the manifest pointing at a
  # file whose actual content does not match the recorded md5 — that's
  # the edited-orphan signature.
  ghost <- file.path(out_dir, "ghost.md")
  writeLines("edited content", ghost)
  m_path <- file.path(out_dir, ".svt_manifest.json")
  m <- yaml::yaml.load(paste(readLines(m_path), collapse = "\n"))
  m$artifacts[[length(m$artifacts) + 1L]] <- list(
    path = "ghost.md",
    md5 = "00000000000000000000000000000000"
  )
  arts <- vapply(m$artifacts, function(a) {
    paste0("{\"path\":\"", a$path, "\",\"md5\":\"", a$md5, "\"}")
  }, character(1))
  writeLines(paste0("{\"schema_version\":\"1.0\",\"artifacts\":[",
                    paste(arts, collapse = ","), "]}"),
             m_path)

  expect_error(svt_render(feats, inv, out_dir = out_dir),
               "manual edits")
  expect_true(file.exists(ghost))  # not deleted because abort came first
})

test_that("delete_pristine_orphans drops empty <slug>/ subdirs", {
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(out_dir, "x"))
  inv_path <- "x/inventory.json"
  fp <- file.path(out_dir, inv_path)
  writeLines("{}", fp)
  prior <- tibble::tibble(path = inv_path,
                          md5 = unname(tools::md5sum(fp)))

  delete_pristine_orphans(out_dir, prior, planned_paths = character())

  expect_false(file.exists(fp))
  expect_false(dir.exists(file.path(out_dir, "x")))
})

test_that("re-run on identical inputs is idempotent (manifest hash-stable)", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)
  m1 <- paste(readLines(file.path(out_dir, ".svt_manifest.json")),
              collapse = "\n")
  svt_render(feats, inv, out_dir = out_dir)
  m2 <- paste(readLines(file.path(out_dir, ".svt_manifest.json")),
              collapse = "\n")
  expect_identical(m1, m2)
})
