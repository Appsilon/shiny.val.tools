test_that("enumerate_app_files finds the conventional starting set in the traditional_basic fixture", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  expect_true(nzchar(app_path), info = "fixture missing from inst/extdata/")

  files <- enumerate_app_files(app_path)

  expect_type(files, "character")
  expect_setequal(
    files,
    c("R/helpers.R", "R/utils/format.R", "global.R", "server.R", "ui.R")
  )
})

test_that("enumerate_app_files returns paths in deterministic (sorted) order", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  files <- enumerate_app_files(app_path)

  expect_identical(files, sort(files))
})

test_that("enumerate_app_files uses forward slashes regardless of platform", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  files <- enumerate_app_files(app_path)

  expect_false(any(grepl("\\\\", files)),
               info = "paths must not contain backslashes — determinism contract")
})

test_that("enumerate_app_files follows source() to files outside the starting set", {
  app_path <- system.file("extdata", "traditional_with_source",
                          package = "shiny.val.tools")

  files <- enumerate_app_files(app_path)

  # helpers/* are reachable only via source(), not by R/**/*.R
  expect_true("helpers/foo.R" %in% files)
  expect_true("helpers/bar.R" %in% files)
  expect_true("helpers/dev.R" %in% files)
  # the starting set is still there
  expect_true(all(c("global.R", "ui.R", "server.R") %in% files))
})

test_that("enumerate_app_files terminates on source() cycles without infinite recursion", {
  app_path <- system.file("extdata", "cycle_app", package = "shiny.val.tools")

  files <- enumerate_app_files(app_path)

  expect_setequal(files, c("a.R", "app.R", "b.R"))
})

test_that("enumerate_starting_set adds .Rprofile and app/main.R when present", {
  app_path <- materialize_fixture_with_dotfiles("rhino_basic")

  files <- enumerate_starting_set(app_path)

  expect_true(".Rprofile" %in% files)
  expect_true("app/main.R" %in% files)
  # Rhino's app.R (rhino::app() entry) is still in the starting set.
  expect_true("app.R" %in% files)
})

test_that("enumerate_app_files follows absolute box paths through a rhino app", {
  app_path <- materialize_fixture_with_dotfiles("rhino_basic")

  files <- enumerate_app_files(app_path)

  # app/main.R imports app/view/home via absolute path.
  expect_true("app/main.R" %in% files)
  expect_true("app/view/home.R" %in% files)
})
