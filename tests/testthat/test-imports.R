test_that("find_library_calls detects unquoted and quoted library/require", {
  parsed <- parse(text = "
    library(shiny)
    library('dplyr')
    require(survival)
    require('ggplot2')
  ", keep.source = TRUE)

  calls <- find_library_calls(parsed)

  expect_length(calls, 4)
  kinds <- vapply(calls, function(c) c$kind, character(1))
  pkgs  <- vapply(calls, function(c) c$package, character(1))
  expect_equal(kinds, c("library", "library", "require", "require"))
  expect_equal(pkgs, c("shiny", "dplyr", "survival", "ggplot2"))
})

test_that("find_library_calls handles named `package` argument", {
  parsed <- parse(text = "
    library(package = shiny)
    library(package = 'dplyr')
  ", keep.source = TRUE)

  calls <- find_library_calls(parsed)

  expect_length(calls, 2)
  expect_equal(vapply(calls, function(c) c$package, character(1)),
               c("shiny", "dplyr"))
})

test_that("find_library_calls skips dynamic forms (character.only = TRUE on a name)", {
  parsed <- parse(text = "
    pkg <- 'dplyr'
    library(pkg, character.only = TRUE)
  ", keep.source = TRUE)

  calls <- find_library_calls(parsed)

  # `library(pkg, character.only = TRUE)` cannot be statically resolved.
  expect_length(calls, 0)
})

test_that("find_library_calls flags inside_function calls", {
  parsed <- parse(text = "
    library(shiny)
    f <- function() {
      library(survival)
    }
  ", keep.source = TRUE)

  calls <- find_library_calls(parsed)

  expect_length(calls, 2)
  inside <- vapply(calls, function(c) c$inside_function, logical(1))
  pkgs   <- vapply(calls, function(c) c$package, character(1))
  expect_equal(pkgs, c("shiny", "survival"))
  expect_equal(inside, c(FALSE, TRUE))
})

test_that("find_library_calls does not flag direct calls inside a Shiny server function", {
  # The server function is still a function-body, so SVT-W008 fires
  # when library() is called inside ANY function. We verify the flag works.
  parsed <- parse(text = "
    function(input, output, session) {
      library(dplyr)
    }
  ", keep.source = TRUE)

  calls <- find_library_calls(parsed)

  expect_length(calls, 1)
  expect_true(calls[[1]]$inside_function)
})

test_that("build_imports_table is a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  imports <- build_imports_table(app_path)

  expect_s3_class(imports, "tbl_df")
  expect_named(
    imports,
    c("from_file", "kind", "package", "alias", "function_set", "whole_namespace",
      "local_path", "absolute", "conditional", "inside_function", "line", "col"),
    ignore.order = TRUE
  )
})

test_that("build_imports_table picks up library(shiny) from ui.R and global.R", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  imports <- build_imports_table(app_path)

  shiny_rows <- imports[imports$package == "shiny", ]
  expect_setequal(shiny_rows$from_file, c("ui.R", "global.R"))
  expect_true(all(shiny_rows$kind == "library"))
  expect_true(all(!shiny_rows$inside_function))
})

test_that("build_imports_table function_set is a list-column (empty for library/require)", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  imports <- build_imports_table(app_path)

  expect_type(imports$function_set, "list")
  # All library/require rows: no function-set narrowing.
  for (fs in imports$function_set) expect_length(fs, 0)
})
