test_that("svt_render writes index.md with summary, features, modules sections", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  result <- svt_render(feats, inv, out_dir = out_dir)

  expect_true(file.exists(file.path(out_dir, "index.md")))
  expect_true(file.exists(file.path(out_dir, "index.html")))
  expect_true(any(grepl("index\\.md$", result$index)))
  expect_true(any(grepl("index\\.html$", result$index)))

  txt <- paste(readLines(file.path(out_dir, "index.md")), collapse = "\n")
  expect_match(txt, "^# .* Validation", perl = TRUE)
  expect_match(txt, "## Summary", fixed = TRUE)
  expect_match(txt, "## App overview", fixed = TRUE)
  expect_match(txt, "[Architecture diagram](index.html)", fixed = TRUE)
  expect_match(txt, "## Features", fixed = TRUE)
  expect_match(txt, "## Modules", fixed = TRUE)
  expect_match(txt, "## Module relationships", fixed = TRUE)
  expect_match(txt, "## Aggregate warnings", fixed = TRUE)
  expect_match(txt, "## Reviewers", fixed = TRUE)
})

test_that("index.md modules table links use slugified paths", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  txt <- paste(readLines(file.path(out_dir, "index.md")), collapse = "\n")
  # The logical name (with slashes) appears in the Name column;
  # the link uses the slug.
  expect_match(txt, "app/view/mod_a", fixed = TRUE)
  expect_match(txt, "[app--view--mod_a.md](app--view--mod_a.md)", fixed = TRUE)
  expect_match(txt, "[app--view--mod_a.html](app--view--mod_a.html)",
               fixed = TRUE)
})

test_that("compute_module_edges links a parent to its child modules", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)

  edges <- compute_module_edges(feats$records, graph)
  expect_s3_class(edges, "tbl_df")
  expect_setequal(colnames(edges), c("parent", "child"))
})

test_that("index.html is self-contained and leaves no _files/ libdir", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  expect_true(file.exists(file.path(out_dir, "index.html")))
  expect_false(dir.exists(file.path(out_dir, "index_files")))
  txt <- paste(readLines(file.path(out_dir, "index.html"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "base64", fixed = TRUE)
  expect_false(grepl("_files/", txt, fixed = TRUE))
})

test_that("index.md Reviewers section is preserved across regeneration", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  index_path <- file.path(out_dir, "index.md")
  txt <- paste(readLines(index_path), collapse = "\n")
  edited <- sub(
    "- Developer: __________________ Date: __________",
    "- Developer: Vedha Date: 2026-04-29",
    txt, fixed = TRUE
  )
  writeLines(edited, index_path)
  # Update the manifest hash to match the edit so the lifecycle treats
  # it as a same-path edited file (not an orphan, since index.md is in
  # the planned set).
  m_path <- file.path(out_dir, ".svt_manifest.json")
  m <- yaml::yaml.load(paste(readLines(m_path), collapse = "\n"))
  for (i in seq_along(m$artifacts)) {
    if (identical(m$artifacts[[i]]$path, "index.md")) {
      m$artifacts[[i]]$md5 <- unname(tools::md5sum(index_path))
    }
  }
  arts <- vapply(m$artifacts, function(a) {
    paste0("{\"path\":\"", a$path, "\",\"md5\":\"", a$md5, "\"}")
  }, character(1))
  writeLines(paste0("{\"schema_version\":\"1.0\",\"artifacts\":[",
                    paste(arts, collapse = ","), "]}"),
             m_path)

  svt_render(feats, inv, out_dir = out_dir)
  txt2 <- paste(readLines(index_path), collapse = "\n")
  expect_match(txt2, "Vedha Date: 2026-04-29", fixed = TRUE)
})

test_that("index.html points its click handler at slugified artifact urls", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  svt_render(feats, inv, out_dir = out_dir)

  txt <- paste(readLines(file.path(out_dir, "index.html"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "app--view--mod_a.html", fixed = TRUE)
  expect_match(txt, "selectNode", fixed = TRUE)
})
