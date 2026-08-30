fixture <- function(name) fixture_path(name)

surfaces_for <- function(app, manifest = NULL) {
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture(app)))
    feats <- svt_slice(graph, manifest = manifest)
    inv <- svt_inventory(graph, feats)
    svt_test_surface(feats, inv)
  })
}

test_that("scaffold file names follow the test-svt-<slug> rule", {
  plan <- scaffold_plan(surfaces_for("modulepkg_basic")$surfaces)

  expect_true("tests/test-svt-counter_server.R" %in% plan$rel_path)
  expect_equal(plan$kind[plan$rel_path == "tests/test-svt-counter_server.R"],
               "surface")
})

test_that("a helper group only gets a file when the surface has helpers", {
  plan <- scaffold_plan(surfaces_for("traditional_basic")$surfaces)
  expect_true("tests/test-svt-helpers-doubled.R" %in% plan$rel_path)

  plan2 <- scaffold_plan(surfaces_for("modulepkg_basic")$surfaces)
  expect_false(any(grepl("helpers", plan2$rel_path)))
})

test_that("every generated scaffold is syntactically valid R", {
  out <- withr::local_tempdir()
  surf <- surfaces_for("modulepkg_basic")
  res <- svt_scaffold_tests(surf, out_dir = out)

  expect_true(nrow(res) > 0)
  for (p in res$path) {
    expect_no_error(parse(p))
  }
})

test_that("a module scaffold drives testServer with the module's server function", {
  out <- withr::local_tempdir()
  surf <- surfaces_for("modulepkg_basic")
  svt_scaffold_tests(surf, out_dir = out)

  text <- paste(readLines(file.path(out, "tests", "test-svt-counter_server.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "@covers module: counter_server", fixed = TRUE)
  expect_match(text, "SVT scaffold", fixed = TRUE)
  expect_match(text, "testServer(", fixed = TRUE)
  expect_match(text, "args = list(id =", fixed = TRUE)
  # stimuli, observables, internals and terminals all appear as plumbing
  expect_match(text, "session$setInputs(", fixed = TRUE)
  expect_match(text, "step\\s+= NULL")
  expect_match(text, "output$total", fixed = TRUE)
  expect_match(text, "running()", fixed = TRUE)
  expect_match(text, "R--readout_mod.md", fixed = TRUE)
})

test_that("a package-target module scaffold loads the package that exports it", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("modulepkg_basic"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests", "test-svt-counter_server.R"),
                          warn = FALSE), collapse = "\n")
  expect_match(text, "library(modulepkg.basic)", fixed = TRUE)
})

test_that("a box-based module scaffold uses box::use and alias$server", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("rhino_multi_module"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests",
                                    "test-svt-app--view--mod_b.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "box::use(app/view/mod_b)", fixed = TRUE)
  expect_match(text, "testServer(mod_b$server", fixed = TRUE)
})

test_that("a traditional module scaffold sources its defining file", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("traditional_module"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests", "test-svt-R--counter.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, 'source(file.path(app_root, "R/counter.R"))', fixed = TRUE)
  expect_match(text, "testServer(counter_server", fixed = TRUE)
})

test_that("a feature scaffold drives the app directory through testServer", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("dynamic_ui"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests", "test-svt-summary.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "@covers feature: summary", fixed = TRUE)
  expect_match(text, 'testServer(testthat::test_path("..", "..")', fixed = TRUE)
  expect_match(text, "dataset\\s+= NULL")
  expect_match(text, "# drives: summary", fixed = TRUE)
})

test_that("an opaque observable is commented with the redirection, not an equality", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("dynamic_ui"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests", "test-svt-controls.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "SVT-W312", fixed = TRUE)
  expect_match(text, "opaque", fixed = TRUE)
})

test_that("helper stubs carry one test_that per helper, method category first", {
  out <- withr::local_tempdir()
  manifest <- list(
    features = list(list(
      name = "doubled",
      roots = list(list(output = "doubled")),
      package_categories = list(app = "method")
    )),
    modules = list()
  )
  svt_scaffold_tests(surfaces_for("traditional_basic", manifest), out_dir = out)
  text <- paste(readLines(file.path(out, "tests",
                                    "test-svt-helpers-doubled.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "@covers fn: double_it", fixed = TRUE)
  expect_match(text, "Category: method", fixed = TRUE)
  expect_match(text, "R/helpers.R:1", fixed = TRUE)
})

test_that("scaffolds are byte-identical across runs", {
  out1 <- withr::local_tempdir()
  out2 <- withr::local_tempdir()
  surf <- surfaces_for("modulepkg_basic")

  a <- svt_scaffold_tests(surf, out_dir = out1)
  b <- svt_scaffold_tests(surf, out_dir = out2)

  expect_equal(unname(tools::md5sum(a$path)), unname(tools::md5sum(b$path)))
})

test_that("an edited staging scaffold is never rewritten", {
  out <- withr::local_tempdir()
  surf <- surfaces_for("modulepkg_basic")
  svt_scaffold_tests(surf, out_dir = out)

  path <- file.path(out, "tests", "test-svt-counter_server.R")
  writeLines("# a real test somebody wrote", path)

  res <- svt_scaffold_tests(surf, out_dir = out)

  expect_equal(readLines(path, warn = FALSE), "# a real test somebody wrote")
  expect_equal(res$status[res$path == path], "skipped_edited")
})

test_that("a pristine staging scaffold refreshes", {
  out <- withr::local_tempdir()
  surf <- surfaces_for("modulepkg_basic")
  svt_scaffold_tests(surf, out_dir = out)

  res <- svt_scaffold_tests(surf, out_dir = out)
  expect_true(all(res$status %in% c("written", "unchanged")))
})

test_that("target = 'app' never overwrites an existing path and warns SVT-W309", {
  app <- withr::local_tempdir()
  file.copy(list.files(fixture("traditional_basic"), full.names = TRUE), app,
            recursive = TRUE)
  dir.create(file.path(app, "tests", "testthat"), recursive = TRUE)
  existing <- file.path(app, "tests", "testthat", "test-svt-doubled.R")
  writeLines("# hand written", existing)

  surf <- with_svt_cache({
    graph <- svt_build_graph(svt_parse(app))
    feats <- svt_slice(graph)
    svt_test_surface(feats, svt_inventory(graph, feats))
  })

  res <- svt_scaffold_tests(surf, out_dir = withr::local_tempdir(),
                            target = "app")

  expect_equal(readLines(existing, warn = FALSE), "# hand written")
  expect_equal(res$status[basename(res$path) == "test-svt-doubled.R"],
               "skipped_exists")
  expect_true("SVT-W309" %in% unlist(res$warnings))
})

test_that("the features argument scaffolds a subset", {
  out <- withr::local_tempdir()
  res <- svt_scaffold_tests(surfaces_for("dynamic_ui"), out_dir = out,
                            features = "summary")

  expect_false(any(grepl("controls", res$path)))
  expect_true(any(grepl("summary", res$path)))
})

test_that("svt_render(scaffold = TRUE) tracks the scaffolds in the manifest", {
  out <- withr::local_tempdir()
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture("traditional_basic")))
    feats <- svt_slice(graph)
    inv <- svt_inventory(graph, feats)
    surf <- svt_test_surface(feats, inv)

    res <- svt_render(feats, inv, out_dir = out, surface = surf,
                      scaffold = TRUE)

    expect_true(file.exists(file.path(out, "tests", "test-svt-doubled.R")))
    tracked <- read_artifact_manifest(out)$path
    expect_true("tests/test-svt-doubled.R" %in% tracked)
    expect_true(length(res$scaffolds) > 0)
  })
})

test_that("svt_validate(scaffold = TRUE) writes scaffolds, and is off by default", {
  out <- withr::local_tempdir()
  svt_validate(fixture("traditional_basic"), out_dir = out)
  expect_false(dir.exists(file.path(out, "tests")))

  out2 <- withr::local_tempdir()
  svt_validate(fixture("traditional_basic"), out_dir = out2, scaffold = TRUE)
  expect_true(file.exists(file.path(out2, "tests", "test-svt-doubled.R")))
})

test_that("svt_scaffold_tests requires an svt_test_surface", {
  expect_error(svt_scaffold_tests(list()), "svt_test_surface")
})

test_that("a parameterised module scaffold names every wrapper formal", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("rhino_multi_module"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests",
                                    "test-svt-app--view--mod_b.R"),
                          warn = FALSE), collapse = "\n")

  # mod_b's server is function(id, selected): a scaffold naming only `id`
  # would not run.
  expect_match(text, 'args = list(id = "test", selected = NULL)', fixed = TRUE)
})

test_that("a module with no inputs says so rather than blaming blockers", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("rhino_multi_module"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests",
                                    "test-svt-app--view--mod_b.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "driven by its arguments", fixed = TRUE)
  expect_false(grepl("blockers", text, fixed = TRUE))
})

test_that("a traditional module scaffold attaches shiny before sourcing", {
  out <- withr::local_tempdir()
  svt_scaffold_tests(surfaces_for("traditional_module"), out_dir = out)
  text <- paste(readLines(file.path(out, "tests", "test-svt-R--counter.R"),
                          warn = FALSE), collapse = "\n")

  expect_match(text, "library(shiny)", fixed = TRUE)
  expect_lt(regexpr("library(shiny)", text, fixed = TRUE),
            regexpr("source(file.path", text, fixed = TRUE))
})

test_that("a generated traditional module scaffold runs once its assertions are filled", {
  skip_if_not_installed("shiny")

  # Work on a copy: the scaffold resolves its app root with
  # testthat::test_path("..", ".."), so it has to sit in <app>/tests/testthat.
  app <- file.path(withr::local_tempdir(), "app")
  dir.create(app, recursive = TRUE)
  file.copy(list.files(fixture("traditional_module"), full.names = TRUE),
            app, recursive = TRUE)

  surf <- with_svt_cache({
    graph <- svt_build_graph(svt_parse(app))
    feats <- svt_slice(graph)
    svt_test_surface(feats, svt_inventory(graph, feats))
  })
  out <- withr::local_tempdir()
  svt_scaffold_tests(surf, out_dir = out)

  text <- readLines(file.path(out, "tests", "test-svt-R--counter.R"),
                    warn = FALSE)
  # Do what a developer does: drop the skip, seed the stimulus, assert.
  filled <- text[!grepl("SVT scaffold", text, fixed = TRUE)]
  filled <- sub("^(\\s+)step\\s+= NULL.*$", "\\1step = 3", filled)
  filled <- sub("^\\s+# expect_equal\\(running\\(\\).*$",
                '    expect_equal(running(), 6)', filled)
  filled <- sub("^\\s+# expect_equal\\(output\\$total.*$",
                '    expect_equal(output$total, "count: 6")', filled)

  staged <- file.path(app, "tests", "testthat")
  dir.create(staged, recursive = TRUE)
  target <- file.path(staged, "test-filled.R")
  writeLines(filled, target)

  res <- testthat::test_file(target, reporter = "silent")
  outcomes <- unlist(lapply(res, function(r) r$results), recursive = FALSE)
  expect_gt(length(outcomes), 0)
  expect_true(all(vapply(outcomes,
                         function(r) inherits(r, "expectation_success"),
                         logical(1))))
})

test_that("a scaffold never contains a double blank line", {
  out <- withr::local_tempdir()
  res <- svt_scaffold_tests(surfaces_for("rhino_multi_module"), out_dir = out)

  for (p in res$path) {
    lines <- readLines(p, warn = FALSE)
    blank <- !nzchar(trimws(lines))
    expect_false(any(blank & c(FALSE, blank[-length(blank)])),
                 info = basename(p))
  }
})
