test_that("find_references emits one row per input$x read", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$a <- renderText({ input$x + input$y })
      output$b <- renderText({ input$x })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  inputs <- Filter(function(r) r$kind == "input", refs)

  # One row per read site, not per unique name.
  expect_length(inputs, 3)
  expect_setequal(unique(vapply(inputs, function(r) r$name, character(1))),
                  c("x", "y"))
})

test_that("find_references attributes input$x reads to the enclosing definition", {
  parsed <- parse(text = "
    function(input, output, session) {
      total <- reactive({ input$x })
      output$z <- renderText({ total() + input$y })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  inputs <- Filter(function(r) r$kind == "input", refs)
  by_name <- split(inputs, vapply(inputs, function(r) r$name, character(1)))

  expect_equal(by_name$x[[1]]$in_def_kind, "reactive")
  expect_equal(by_name$x[[1]]$in_def_name, "total")

  expect_equal(by_name$y[[1]]$in_def_kind, "output")
  expect_equal(by_name$y[[1]]$in_def_name, "z")
})

test_that("find_references emits a 'call' row for named-reactive reads", {
  parsed <- parse(text = "
    function(input, output, session) {
      total <- reactive({ input$x })
      output$z <- renderText({ total() + 1 })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  calls <- Filter(function(r) r$kind == "call", refs)
  total_calls <- Filter(function(r) r$name == "total", calls)

  expect_length(total_calls, 1)
  expect_equal(total_calls[[1]]$in_def_kind, "output")
  expect_equal(total_calls[[1]]$in_def_name, "z")
})

test_that("find_references skips dynamic input[[expr]] reads", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$z <- renderText({ input[[paste0('x', i)]] })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  inputs <- Filter(function(r) r$kind == "input", refs)
  expect_length(inputs, 0)
})

test_that("find_references namespaces input reads inside a moduleServer body", {
  parsed <- parse(text = "
    counter_server <- function(id) {
      moduleServer(id, function(input, output, session) {
        output$z <- renderText({ input$x })
      })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  ins <- Filter(function(r) r$kind == "input", refs)

  expect_length(ins, 1)
  expect_equal(ins[[1]]$namespace, "counter_server")
  expect_equal(ins[[1]]$in_def_namespace, "counter_server")
})

test_that("build_references_table returns a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  refs <- build_references_table(app_path)

  expect_s3_class(refs, "tbl_df")
  expect_named(
    refs,
    c("from_file", "kind", "name", "namespace", "container", "package",
      "internal", "in_def_kind", "in_def_name", "in_def_namespace",
      "line", "col"),
    ignore.order = TRUE
  )
})

test_that("build_references_table picks up input$x in traditional_basic", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  refs <- build_references_table(app_path)

  in_rows <- refs[refs$kind == "input", ]
  expect_equal(nrow(in_rows), 1L)
  expect_equal(in_rows$name, "x")
  expect_equal(in_rows$from_file, "server.R")
  expect_equal(in_rows$in_def_kind, "output")
  expect_equal(in_rows$in_def_name, "doubled")
})

test_that("find_references emits 'value' rows for reads of rv$name (rv known to be reactiveValues)", {
  parsed <- parse(text = "
    function(input, output, session) {
      rv <- reactiveValues(x = 0, y = 0)
      output$z <- renderText({ rv$x + rv$y })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  vals <- Filter(function(r) r$kind == "value", refs)

  expect_length(vals, 2)
  expect_setequal(vapply(vals, function(r) r$name, character(1)), c("x", "y"))
  expect_true(all(vapply(vals, function(r) r$container, character(1)) == "rv"))
})

test_that("find_references does NOT emit value rows for non-reactiveValues containers", {
  parsed <- parse(text = "
    function(input, output, session) {
      lst <- list(x = 1, y = 2)
      output$z <- renderText({ lst$x + lst$y })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  vals <- Filter(function(r) r$kind == "value", refs)
  expect_length(vals, 0)
})

test_that("find_references emits 'call' rows for pkg::fn(...) calls with package set", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$z <- renderText({ dplyr::filter(df, x > 1) })
    }
  ", keep.source = TRUE)

  refs <- find_references(parsed)
  filt <- Filter(function(r) r$kind == "call" && r$name == "filter", refs)

  expect_length(filt, 1)
  expect_equal(filt[[1]]$package, "dplyr")
  expect_equal(filt[[1]]$in_def_kind, "output")
  expect_equal(filt[[1]]$in_def_name, "z")
})

test_that("shiny::moduleServer() establishes the module namespace like the bare form", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "mod_server <- function(id) {",
    "  shiny::moduleServer(id, function(input, output, session) {",
    "    doubled <- shiny::reactive({ input$n * 2 })",
    "    output$txt <- shiny::renderText({ doubled() })",
    "  })",
    "}"
  ), file.path(tmp, "app.R"))

  refs <- build_references_table(tmp)
  read_row <- refs[refs$name == "doubled" & refs$kind == "call", ]

  expect_equal(nrow(read_row), 1L)
  # The qualified form must not be swallowed by the `pkg::fn()` branch.
  expect_equal(read_row$in_def_namespace, "app")
})
