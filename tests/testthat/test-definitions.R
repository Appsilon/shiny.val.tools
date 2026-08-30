test_that("find_definitions emits one row per top-level output$x <- ...", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$a <- renderText('a')
      output$b <- renderPlot({ plot(1) })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  outputs <- Filter(function(d) d$kind == "output", defs)

  expect_length(outputs, 2)
  expect_equal(vapply(outputs, function(d) d$name, character(1)), c("a", "b"))
  expect_true(all(vapply(outputs, function(d) is.na(d$namespace), logical(1))))
})

test_that("find_definitions records line/col for output assignments", {
  parsed <- parse(text = "function(input, output, session) {\n  output$x <- renderText('x')\n}",
                  keep.source = TRUE)

  defs <- find_definitions(parsed)
  outputs <- Filter(function(d) d$kind == "output", defs)

  expect_length(outputs, 1)
  expect_equal(outputs[[1]]$line, 2L)
  expect_true(is.integer(outputs[[1]]$col) && !is.na(outputs[[1]]$col))
})

test_that("find_definitions ignores non-output assignments", {
  parsed <- parse(text = "
    x <- 1
    foo <- function() 2
    y$z <- 3
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  outputs <- Filter(function(d) d$kind == "output", defs)

  expect_length(outputs, 0)
})

test_that("find_definitions handles output[['x']] <- ... as a static assignment", {
  parsed <- parse(text = "output[['name']] <- renderText('hi')", keep.source = TRUE)

  defs <- find_definitions(parsed)
  outputs <- Filter(function(d) d$kind == "output", defs)

  expect_length(outputs, 1)
  expect_equal(outputs[[1]]$name, "name")
})

test_that("find_definitions skips dynamic output IDs (output[[expr]] <- ...)", {
  parsed <- parse(text = "
    output[[paste0('a', i)]] <- renderText('hi')
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  outputs <- Filter(function(d) d$kind == "output", defs)

  expect_length(outputs, 0)
})

test_that("build_definitions_table returns a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  defs <- build_definitions_table(app_path)

  expect_s3_class(defs, "tbl_df")
  expect_named(
    defs,
    c("from_file", "kind", "name", "namespace", "container", "def_call",
      "line", "col", "wrapper_binding", "wrapper_formals"),
    ignore.order = TRUE
  )
})

test_that("build_definitions_table picks up output$doubled in traditional_basic", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  defs <- build_definitions_table(app_path)

  output_rows <- defs[defs$kind == "output", ]
  expect_equal(nrow(output_rows), 1L)
  expect_equal(output_rows$name, "doubled")
  expect_equal(output_rows$from_file, "server.R")
})

test_that("find_definitions emits a 'reactive' row per named reactive binding", {
  parsed <- parse(text = "
    function(input, output, session) {
      x <- reactive({ input$a + 1 })
      y <- eventReactive(input$go, { input$b })
      z <- bindEvent(reactive({ input$c }), input$go)
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  reacts <- Filter(function(d) d$kind == "reactive", defs)

  expect_length(reacts, 3)
  expect_equal(vapply(reacts, function(d) d$name, character(1)), c("x", "y", "z"))
})

test_that("find_definitions emits an 'observer' row per named observer binding", {
  parsed <- parse(text = "
    function(input, output, session) {
      o1 <- observe({ print(input$a) })
      o2 <- observeEvent(input$go, { print(input$b) })
      o3 <- bindEvent(observe({ print(input$c) }), input$go)
      d  <- downloadHandler(filename = function() 'x.csv',
                            content  = function(file) write.csv(1, file))
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  obs <- Filter(function(d) d$kind == "observer", defs)

  expect_length(obs, 4)
  expect_equal(vapply(obs, function(d) d$name, character(1)),
               c("o1", "o2", "o3", "d"))
})

test_that("find_definitions ignores anonymous reactive() / observe() in v1", {
  parsed <- parse(text = "
    function(input, output, session) {
      observe({ print(input$a) })
      reactive({ input$b })
      output$z <- renderText({ reactive({ input$c })() })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  named <- Filter(function(d) d$kind %in% c("reactive", "observer"), defs)

  expect_length(named, 0)
})

test_that("find_definitions does not classify a regular helper as reactive/observer", {
  parsed <- parse(text = "
    helper <- function(x) x + 1
    val    <- 42
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  # `helper` is recorded as a top-level function definition (spec 06 helper
  # source), but never as a reactive or an observer.
  expect_length(Filter(function(d) d$kind %in% c("reactive", "observer"), defs), 0)
  expect_equal(vapply(defs, function(d) d$kind, character(1)), "function")
})

test_that("find_definitions emits a value per named entry of reactiveValues()", {
  parsed <- parse(text = "
    function(input, output, session) {
      rv <- reactiveValues(a = NULL, b = 0)
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  values <- Filter(function(d) d$kind == "value", defs)

  expect_length(values, 2)
  expect_equal(vapply(values, function(d) d$name, character(1)), c("a", "b"))
  expect_equal(vapply(values, function(d) d$container, character(1)), c("rv", "rv"))
})

test_that("find_definitions emits a value on subsequent named writes (rv$name <- ...)", {
  parsed <- parse(text = "
    function(input, output, session) {
      rv <- reactiveValues(a = NULL)
      observe({
        rv$a <- 1
        rv$b <- 2
        rv[['c']] <- 3
      })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  values <- Filter(function(d) d$kind == "value", defs)

  # Each assignment is one row — the walker emits per-write definitions
  # so later passes can pick first-seen as the canonical site.
  vnames <- vapply(values, function(d) d$name, character(1))
  expect_setequal(vnames, c("a", "b", "c"))
})

test_that("find_definitions ignores writes to non-rv containers", {
  parsed <- parse(text = "
    function(input, output, session) {
      lst <- list(a = 1)
      lst$a <- 2
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  expect_length(defs, 0)
})

test_that("find_definitions skips dynamic rv[[expr]] writes", {
  parsed <- parse(text = "
    function(input, output, session) {
      rv <- reactiveValues()
      observe({ rv[[paste0('x', i)]] <- 1 })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  values <- Filter(function(d) d$kind == "value", defs)
  expect_length(values, 0)
})

test_that("find_definitions emits a module_server row at every moduleServer() site", {
  parsed <- parse(text = "
    counter_server <- function(id) {
      moduleServer(id, function(input, output, session) {
        output$count <- renderText({ input$x })
      })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  modules <- Filter(function(d) d$kind == "module_server", defs)

  expect_length(modules, 1)
  expect_equal(modules[[1]]$name, "counter_server")
})

test_that("find_definitions namespaces outputs defined inside a moduleServer body", {
  parsed <- parse(text = "
    counter_server <- function(id) {
      moduleServer(id, function(input, output, session) {
        output$count <- renderText({ input$x })
        total <- reactive({ input$x })
      })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)

  out_row <- Filter(function(d) d$kind == "output" && d$name == "count", defs)
  expect_length(out_row, 1)
  expect_equal(out_row[[1]]$namespace, "counter_server")

  reac_row <- Filter(function(d) d$kind == "reactive" && d$name == "total", defs)
  expect_length(reac_row, 1)
  expect_equal(reac_row[[1]]$namespace, "counter_server")
})

test_that("find_definitions recognizes legacy callModule definitions", {
  # Legacy form: function(input, output, session, ...) is the module body;
  # assigning it to a name still defines a module server.
  parsed <- parse(text = "
    legacy_module <- function(input, output, session) {
      output$x <- renderText('hi')
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  modules <- Filter(function(d) d$kind == "module_server", defs)

  expect_length(modules, 1)
  expect_equal(modules[[1]]$name, "legacy_module")

  out_row <- Filter(function(d) d$kind == "output" && d$name == "x", defs)
  expect_length(out_row, 1)
  expect_equal(out_row[[1]]$namespace, "legacy_module")
})

test_that("find_definitions does NOT treat the top-level Shiny server as a module", {
  # The server function `function(input, output, session) { ... }` lives at
  # the top of server.R, unassigned. It is the app server, not a module.
  parsed <- parse(text = "
    function(input, output, session) {
      output$x <- renderText('hi')
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  modules <- Filter(function(d) d$kind == "module_server", defs)
  expect_length(modules, 0)

  outs <- Filter(function(d) d$kind == "output", defs)
  expect_length(outs, 1)
  # Top-level server's outputs have no namespace.
  expect_true(is.na(outs[[1]]$namespace))
})

test_that("find_definitions records def_call for output and reactive kinds", {
  parsed <- parse(text = "
    function(input, output, session) {
      output$a <- renderText('a')
      output$b <- shiny::renderPlot({ plot(1) })
      r <- reactive({ 1 })
      e <- eventReactive(input$go, { 2 })
      o <- observeEvent(input$go, { 3 })
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  by_name <- function(nm) Filter(function(d) identical(d$name, nm), defs)[[1L]]

  expect_equal(by_name("a")$def_call, "renderText")
  expect_equal(by_name("b")$def_call, "renderPlot")
  expect_equal(by_name("r")$def_call, "reactive")
  expect_equal(by_name("e")$def_call, "eventReactive")
  expect_equal(by_name("o")$def_call, "observeEvent")
})

test_that("def_call unwraps bindEvent to the delegated call", {
  parsed <- parse(text = "
    function(input, output, session) {
      r <- bindEvent(reactive({ 1 }), input$go)
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  r <- Filter(function(d) identical(d$name, "r"), defs)[[1L]]
  expect_equal(r$kind, "reactive")
  expect_equal(r$def_call, "reactive")
})

test_that("find_definitions emits kind = 'function' for top-level function defs", {
  parsed <- parse(text = "
    double_it <- function(x) x * 2
    make_label <- function(x) {
      paste0('n = ', x)
    }
  ", keep.source = TRUE)

  fns <- Filter(function(d) d$kind == "function", find_definitions(parsed))

  expect_equal(vapply(fns, function(d) d$name, character(1)),
               c("double_it", "make_label"))
  expect_equal(fns[[1]]$line, 2L)
  expect_true(all(vapply(fns, function(d) is.na(d$namespace), logical(1))))
  expect_true(all(vapply(fns, function(d) is.na(d$def_call), logical(1))))
})

test_that("a function definition nested inside another function is not a helper row", {
  parsed <- parse(text = "
    outer <- function(x) {
      inner <- function(y) y + 1
      inner(x)
    }
  ", keep.source = TRUE)

  fns <- Filter(function(d) d$kind == "function", find_definitions(parsed))
  expect_equal(vapply(fns, function(d) d$name, character(1)), "outer")
})

test_that("a module server function is a module_server row, not a function row", {
  parsed <- parse(text = "
    mod_server <- function(input, output, session) {
      output$x <- renderText('x')
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed)
  expect_length(Filter(function(d) d$kind == "function", defs), 0)
  expect_length(Filter(function(d) d$kind == "module_server", defs), 1)
})

test_that("build_definitions_table carries def_call and function rows", {
  app <- fixture_path("traditional_basic")
  defs <- with_svt_cache(build_definitions_table(app))

  expect_true("def_call" %in% names(defs))
  doubled <- defs[defs$kind == "output" & defs$name == "doubled", ]
  expect_equal(doubled$def_call, "renderText")

  helpers <- defs[defs$kind == "function", ]
  expect_true("double_it" %in% helpers$name)
  expect_true("R/helpers.R" %in% helpers$from_file)
})

test_that("a module_server row records its wrapper's formals", {
  defs <- with_svt_cache(build_definitions_table(fixture_path("rhino_multi_module")))
  mods <- defs[defs$kind == "module_server", ]

  # mod_b's wrapper is function(id, selected) — a testServer() scaffold that
  # names only `id` would not run.
  expect_equal(mods$wrapper_formals[mods$name == "app/view/mod_b"],
               "id,selected")
  expect_equal(mods$wrapper_formals[mods$name == "app/view/mod_a"], "id")
})

test_that("the legacy module form records its formals too", {
  parsed <- parse(text = "
    mod_server <- function(input, output, session, dataset) {
      output$x <- renderText('x')
    }
  ", keep.source = TRUE)

  defs <- find_definitions(parsed, from_file = "R/mod.R")
  mod <- Filter(function(d) d$kind == "module_server", defs)[[1L]]
  expect_equal(mod$wrapper_formals, "input,output,session,dataset")
})
