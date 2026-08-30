test_that("build_warnings_table returns a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  warnings <- build_warnings_table(app_path)

  expect_s3_class(warnings, "tbl_df")
  expect_named(
    warnings,
    c("code", "file", "line", "col", "message", "node_id"),
    ignore.order = TRUE
  )
})

test_that("traditional_basic emits no warnings", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  warnings <- build_warnings_table(app_path)

  expect_equal(nrow(warnings), 0L)
})

test_that("SVT-W010 fires on the second and later output$x assignment in the same namespace", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(shiny)",
    "fluidPage(textOutput('x'))"
  ), file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$x <- renderText('a')",
    "  output$x <- renderText('b')",
    "  output$x <- renderText('c')",
    "}"
  ), file.path(tmp, "server.R"))

  warnings <- build_warnings_table(tmp)

  w010 <- warnings[warnings$code == "SVT-W010", ]
  expect_equal(nrow(w010), 2L)
  expect_true(all(w010$file == "server.R"))
  # The first assignment is canonical; rows 2 and 3 are flagged.
  expect_setequal(w010$line, c(3L, 4L))
})

test_that("SVT-W008 fires on library() calls inside a function body", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(shiny)",
    "function(input, output, session) {",
    "  library(dplyr)",
    "  library(survival)",
    "}"
  ), file.path(tmp, "server.R"))
  writeLines("library(shiny)", file.path(tmp, "ui.R"))

  warnings <- build_warnings_table(tmp)
  w008 <- warnings[warnings$code == "SVT-W008", ]
  expect_equal(nrow(w008), 2L)
  expect_setequal(w008$line, c(3L, 4L))
})

test_that("SVT-W005 fires on whole-namespace box::use(pkg[...]) imports", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "box::use(",
    "  shiny[...],",
    "  dplyr[filter, mutate],",
    "  ggplot2[...],",
    ")"
  ), file.path(tmp, "app.R"))

  warnings <- build_warnings_table(tmp)
  w005 <- warnings[warnings$code == "SVT-W005", ]
  expect_equal(nrow(w005), 2L)
})

test_that("SVT-W004 fires on conditional source() and conditional box::use()", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(shiny)",
    "if (Sys.getenv('DEBUG') == '1') {",
    "  source('helpers/dev.R')",
    "  box::use(./optional)",
    "}",
    "source('helpers/foo.R')"
  ), file.path(tmp, "app.R"))
  dir.create(file.path(tmp, "helpers"))
  writeLines("# stub", file.path(tmp, "helpers", "foo.R"))
  writeLines("# stub", file.path(tmp, "helpers", "dev.R"))
  writeLines("# stub", file.path(tmp, "optional.R"))

  warnings <- build_warnings_table(tmp)
  w004 <- warnings[warnings$code == "SVT-W004", ]
  # one source() and one box::use(), both inside the if-block
  expect_equal(nrow(w004), 2L)
})

test_that("SVT-W007 fires when source() and box::use() reach the same file", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "source('shared.R')",
    "box::use(./shared)"
  ), file.path(tmp, "app.R"))
  writeLines("# both source() and box::use() point here", file.path(tmp, "shared.R"))

  warnings <- build_warnings_table(tmp)
  w007 <- warnings[warnings$code == "SVT-W007", ]
  # One warning per call site that participates in the duplicate.
  expect_equal(nrow(w007), 2L)
  expect_setequal(w007$line, c(1L, 2L))
})

test_that("SVT-W001 fires on every dynamic input[[expr]] read", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ input[[paste0('x', i)]] })",
    "  output$b <- renderText({ input[[whatever()]] })",
    "  output$c <- renderText({ input$static })",
    "}"
  ), file.path(tmp, "server.R"))

  warnings <- build_warnings_table(tmp)
  w001 <- warnings[warnings$code == "SVT-W001", ]
  expect_equal(nrow(w001), 2L)
  expect_setequal(w001$line, c(2L, 3L))
})

test_that("SVT-W001 does NOT fire for static input[[\"x\"]] reads", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$z <- renderText({ input[['name']] })
    }
  ", keep.source = TRUE)
  refs <- find_references(parsed)
  expect_true(any(vapply(refs, function(r) r$kind == "input", logical(1))))
})

test_that("SVT-W009 fires on do.call / eval / eval(parse(...)) calls", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ do.call('paste', list(1, 2)) })",
    "  output$b <- renderText({ eval(parse(text = 'x + 1')) })",
    "  output$c <- renderText({ paste('plain') })",
    "}"
  ), file.path(tmp, "server.R"))

  warnings <- build_warnings_table(tmp)
  w009 <- warnings[warnings$code == "SVT-W009", ]
  expect_equal(nrow(w009), 3L)
})

test_that("SVT-W003 fires on every read of a known reactiveValues entry", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  rv <- reactiveValues(a = 0)",
    "  output$x <- renderText({ rv$a })",
    "  output$y <- renderText({ rv$a + 1 })",
    "}"
  ), file.path(tmp, "server.R"))

  warnings <- build_warnings_table(tmp)
  w003 <- warnings[warnings$code == "SVT-W003", ]
  expect_equal(nrow(w003), 2L)
  expect_true(all(w003$file == "server.R"))
})

test_that("SVT-W010 does NOT fire for outputs in different namespaces", {
  # Two modules each define output$x — distinct (namespace, name) pairs.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "mod_a <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$x <- renderText('a')",
    "  })",
    "}",
    "mod_b <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$x <- renderText('b')",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  warnings <- build_warnings_table(tmp)
  w010 <- warnings[warnings$code == "SVT-W010", ]
  expect_equal(nrow(w010), 0L)
})

test_that("detect_render_ui_input flags a literal input id created inside renderUI", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$controls <- renderUI({
        numericInput('threshold', 'Threshold', value = 1)
      })
    }
  ", keep.source = TRUE)

  hits <- find_render_ui_inputs(parsed, from_file = "server.R")

  expect_length(hits, 1)
  expect_equal(hits[[1]]$name, "threshold")
  expect_equal(hits[[1]]$in_def_kind, "output")
  expect_equal(hits[[1]]$in_def_name, "controls")
})

test_that("detect_render_ui_input covers insertUI and ns()-wrapped ids", {
  parsed <- parse(text = "
    function(input, output, session) {
      observeEvent(input$add, {
        insertUI('#anchor', ui = actionButton(ns('go'), 'Go'))
      })
    }
  ", keep.source = TRUE)

  hits <- find_render_ui_inputs(parsed, from_file = "server.R")

  expect_equal(vapply(hits, function(h) h$name, character(1)), "go")
})

test_that("an input control outside a render context is not SVT-W002", {
  parsed <- parse(text = "
    fluidPage(
      numericInput('x', 'x', value = 1)
    )
  ", keep.source = TRUE)

  expect_length(find_render_ui_inputs(parsed, from_file = "ui.R"), 0)
})

test_that("a non-literal id inside renderUI is left to SVT-W001, not W002", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$controls <- renderUI({
        numericInput(paste0('x', i), 'x', value = 1)
      })
    }
  ", keep.source = TRUE)

  expect_length(find_render_ui_inputs(parsed, from_file = "server.R"), 0)
})

test_that("the warnings table carries SVT-W002 with an owning node_id", {
  app <- fixture_path("dynamic_ui")
  warns <- with_svt_cache(build_warnings_table(app))

  w002 <- warns[warns$code == "SVT-W002", ]
  expect_equal(nrow(w002), 1L)
  expect_equal(w002$node_id, node_id("output", NA_character_, NA_character_,
                                     "controls"))

  nodes <- with_svt_cache(build_nodes_table(app))
  expect_true(w002$node_id %in% nodes$id)
})

test_that("every warnings-table row carries a node_id column", {
  app <- fixture_path("traditional_basic")
  warns <- with_svt_cache(build_warnings_table(app))
  expect_true("node_id" %in% names(warns))
})
