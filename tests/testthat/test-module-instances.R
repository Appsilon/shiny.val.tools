test_that("find_module_instances emits one record per wrapper call site with a string id", {
  parsed <- parse(text = "
    function(input, output, session) {
      counter_server('c1')
      counter_server('c2')
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))

  expect_length(records, 2)
  expect_equal(vapply(records, function(r) r$wrapper, character(1)),
               c("counter_server", "counter_server"))
  expect_equal(vapply(records, function(r) r$ns_id, character(1)),
               c("c1", "c2"))
})

test_that("find_module_instances skips calls whose id is not a string literal", {
  parsed <- parse(text = "
    function(input, output, session) {
      counter_server(input$which)
      counter_server(paste0('c', i))
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))
  expect_length(records, 0)
})

test_that("find_module_instances ignores non-wrapper calls", {
  parsed <- parse(text = "
    function(input, output, session) {
      paste('a', 'b')
      counter_server('c1')
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))
  expect_length(records, 1)
  expect_equal(records[[1]]$wrapper, "counter_server")
})

test_that("build_nodes_table emits module_instance nodes per wrapper call", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$x })",
    "  })",
    "}",
    "function(input, output, session) {",
    "  counter_server('c1')",
    "  counter_server('c2')",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)

  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 2L)
  expect_setequal(inst$name, c("counter_server", "counter_server"))
  # The instance node's `container` slot carries the concrete ns_id —
  # makes (type, ns, container, name) keys disambiguate call sites.
  expect_setequal(inst$container, c("c1", "c2"))
  # Each instance has a distinct id.
  expect_equal(length(unique(inst$id)), 2L)
})

test_that("module_instance nodes are namespaced by the parent (NA at top level)", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$x })",
    "  })",
    "}",
    "function(input, output, session) {",
    "  counter_server('c1')",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 1L)
  expect_true(is.na(inst$namespace))
  expect_equal(inst$fq_name, "counter_server[c1]")
})

test_that("module_instance nodes inside another module are namespaced by the outer module", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "inner_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$y <- renderText({ input$x })",
    "  })",
    "}",
    "outer_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    inner_server('inner1')",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 1L)
  expect_equal(inst$name, "inner_server")
  expect_equal(inst$namespace, "outer_server")
  expect_equal(inst$container, "inner1")
})
