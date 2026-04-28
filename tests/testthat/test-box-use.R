test_that("find_box_use_calls handles bare package import", {
  parsed <- parse(text = "box::use(shiny)", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "shiny")
  expect_true(is.na(clauses[[1]]$alias))
  expect_length(clauses[[1]]$function_set, 0)
  expect_false(clauses[[1]]$whole_namespace)
  expect_true(is.na(clauses[[1]]$local_path))
})

test_that("find_box_use_calls captures explicit function set", {
  parsed <- parse(text = "box::use(dplyr[filter, mutate])", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "dplyr")
  expect_equal(clauses[[1]]$function_set, c("filter", "mutate"))
  expect_false(clauses[[1]]$whole_namespace)
})

test_that("find_box_use_calls flags whole-namespace pkg[...] imports", {
  parsed <- parse(text = "box::use(shiny[...])", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "shiny")
  expect_true(clauses[[1]]$whole_namespace)
  expect_length(clauses[[1]]$function_set, 0)
})

test_that("find_box_use_calls captures aliased package", {
  parsed <- parse(text = "box::use(d = dplyr)", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "dplyr")
  expect_equal(clauses[[1]]$alias, "d")
  expect_length(clauses[[1]]$function_set, 0)
})

test_that("find_box_use_calls captures aliased function set", {
  parsed <- parse(text = "box::use(d = dplyr[filter])", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "dplyr")
  expect_equal(clauses[[1]]$alias, "d")
  expect_equal(clauses[[1]]$function_set, "filter")
})

test_that("find_box_use_calls captures local module ./mod and ./mod[fn]", {
  parsed <- parse(text = "
    box::use(
      ./view/main,
      ./utils[helper_fn],
    )
  ", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 2)
  expect_true(is.na(clauses[[1]]$package))
  expect_equal(clauses[[1]]$local_path, "./view/main")
  expect_length(clauses[[1]]$function_set, 0)
  expect_equal(clauses[[2]]$local_path, "./utils")
  expect_equal(clauses[[2]]$function_set, "helper_fn")
})

test_that("find_box_use_calls handles ../ paths", {
  parsed <- parse(text = "box::use(../shared/util)", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$local_path, "../shared/util")
  expect_true(is.na(clauses[[1]]$package))
  expect_false(clauses[[1]]$absolute)
})

test_that("find_box_use_calls treats multi-segment bare paths as absolute box paths", {
  parsed <- parse(text = "box::use(app/view/home)", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_true(is.na(clauses[[1]]$package))
  expect_equal(clauses[[1]]$local_path, "app/view/home")
  expect_true(clauses[[1]]$absolute)
})

test_that("find_box_use_calls handles absolute path with explicit function set", {
  parsed <- parse(text = "box::use(app/logic/util[helper])", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$local_path, "app/logic/util")
  expect_equal(clauses[[1]]$function_set, "helper")
  expect_true(clauses[[1]]$absolute)
})

test_that("find_box_use_calls flags ./mod paths as not absolute", {
  parsed <- parse(text = "box::use(./view/main)", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_false(clauses[[1]]$absolute)
})

test_that("find_box_use_calls handles trailing commas (missing args)", {
  parsed <- parse(text = "box::use(shiny[x], )", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  # The trailing comma produces a missing arg that we silently skip.
  expect_length(clauses, 1)
  expect_equal(clauses[[1]]$package, "shiny")
})

test_that("find_box_use_calls flags conditional context", {
  parsed <- parse(text = "
    if (debug) {
      box::use(./debug/tools)
    }
  ", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 1)
  expect_true(clauses[[1]]$conditional)
})

test_that("find_box_use_calls splits a multi-clause block into one record per clause", {
  parsed <- parse(text = "
    box::use(
      shiny[shinyApp, fluidPage],
      dplyr[filter],
      d = data.table,
      ./view/main,
    )
  ", keep.source = TRUE)

  clauses <- find_box_use_calls(parsed)

  expect_length(clauses, 4)
})

test_that("build_imports_table picks up box::use rows from the box_in_R fixture", {
  app_path <- system.file("extdata", "box_in_R", package = "shiny.val.tools")

  imports <- build_imports_table(app_path)

  # box_in_R: R/main.R has 4 box clauses, shared/util.R has 1 box clause = 5
  box_rows <- imports[imports$kind == "box_use", ]
  expect_equal(nrow(box_rows), 5)

  # whole_namespace TRUE only for `shiny[...]` in R/main.R
  whole <- box_rows[box_rows$whole_namespace, ]
  expect_equal(nrow(whole), 1)
  expect_equal(whole$from_file, "R/main.R")
  expect_equal(whole$package, "shiny")

  # alias preserved
  log_row <- box_rows[!is.na(box_rows$alias) & box_rows$alias == "log", ]
  expect_equal(nrow(log_row), 1)
  expect_equal(log_row$package, "logger")

  # local module rows have NA package
  local_rows <- box_rows[!is.na(box_rows$local_path), ]
  expect_equal(nrow(local_rows), 1)
  expect_equal(local_rows$local_path, "../shared/util")
  expect_true(is.na(local_rows$package))
})

test_that("build_imports_table includes whole_namespace, conditional, and absolute columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  imports <- build_imports_table(app_path)

  expect_named(
    imports,
    c("from_file", "kind", "package", "alias", "function_set", "whole_namespace",
      "local_path", "absolute", "conditional", "inside_function", "line", "col"),
    ignore.order = TRUE
  )

  # Library/require rows are never whole_namespace, conditional, or absolute.
  lib_rows <- imports[imports$kind %in% c("library", "require"), ]
  expect_true(all(!lib_rows$whole_namespace))
  expect_true(all(!lib_rows$conditional))
  expect_true(all(is.na(lib_rows$absolute)))
})

test_that("enumerate_app_files follows box::use(../...) into files outside the starting set", {
  app_path <- system.file("extdata", "box_in_R", package = "shiny.val.tools")

  files <- enumerate_app_files(app_path)

  # shared/util.R is reachable ONLY through box::use(../shared/util) in R/main.R.
  expect_true("shared/util.R" %in% files)
  expect_true("R/main.R" %in% files)
})

test_that("find_box_path_options captures options(box.path = getwd())", {
  parsed <- parse(text = "options(box.path = getwd())", keep.source = TRUE)

  hits <- find_box_path_options(parsed)

  expect_length(hits, 1)
  # The value expression is the parsed `getwd()` call.
  expect_true(is.call(hits[[1]]$value_expr))
  expect_equal(as.character(hits[[1]]$value_expr[[1L]]), "getwd")
})

test_that("find_box_path_options captures string-literal box.path values", {
  parsed <- parse(text = "options(box.path = '/srv/app')", keep.source = TRUE)

  hits <- find_box_path_options(parsed)

  expect_length(hits, 1)
  expect_equal(hits[[1]]$value_expr, "/srv/app")
})

test_that("find_box_path_options ignores other options() calls", {
  parsed <- parse(text = "options(stringsAsFactors = FALSE, scipen = 999)",
                  keep.source = TRUE)

  hits <- find_box_path_options(parsed)

  expect_length(hits, 0)
})

test_that("find_box_path_options finds calls inside conditional blocks", {
  parsed <- parse(text = "
    if (rhino_active) {
      options(box.path = getwd())
    }
  ", keep.source = TRUE)

  hits <- find_box_path_options(parsed)

  expect_length(hits, 1)
})

test_that("discover_box_path returns app root for rhino-style getwd() in .Rprofile", {
  app_path <- materialize_fixture_with_dotfiles("rhino_basic")

  expect_equal(discover_box_path(app_path), ".")
})

test_that("discover_box_path returns NULL when .Rprofile is absent", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  expect_null(discover_box_path(app_path))
})

test_that("discover_box_path returns NULL for string-literal box.path (not honored in v1)", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("options(box.path = '/srv/app')", file.path(tmp, ".Rprofile"))

  expect_null(discover_box_path(tmp))
})

test_that("build_imports_table flags absolute box rows in the rhino_basic fixture", {
  app_path <- materialize_fixture_with_dotfiles("rhino_basic")

  imports <- build_imports_table(app_path)

  abs_rows <- imports[imports$kind == "box_use" &
                        !is.na(imports$absolute) & imports$absolute, ]
  expect_equal(nrow(abs_rows), 1)
  expect_equal(abs_rows$from_file, "app/main.R")
  expect_equal(abs_rows$local_path, "app/view/home")
  expect_true(is.na(abs_rows$package))

  # Package box clauses keep absolute = NA (only meaningful for local paths).
  pkg_box_rows <- imports[imports$kind == "box_use" &
                            !is.na(imports$package), ]
  expect_true(all(is.na(pkg_box_rows$absolute)))
})
