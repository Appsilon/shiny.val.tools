test_that("module_slice produces a subgraph for a module's outputs", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$step })",
    "  })",
    "}",
    "function(input, output, session) {",
    "  counter_server(\"c1\")",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  modules <- module_slice(graph, tmp)

  expect_length(modules, 1L)
  m <- modules[[1L]]
  expect_equal(m$name, "server")
  expect_equal(m$kind, "module")
  expect_setequal(m$contract$inputs, "step")
  expect_setequal(m$contract$outputs, "count")
  out_id <- graph$nodes$id[graph$nodes$type == "output" & graph$nodes$name == "count"]
  in_id  <- graph$nodes$id[graph$nodes$type == "input"  & graph$nodes$name == "step"]
  expect_true(out_id %in% m$node_ids)
  expect_true(in_id %in% m$node_ids)
})

test_that("module_slice extracts named returned reactives as part of the contract", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "filter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    selected <- reactive({ input$which })",
    "    list(selected = selected)",
    "  })",
    "}",
    "function(input, output, session) {",
    "  filter_server(\"f1\")",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  modules <- module_slice(graph, tmp)

  expect_length(modules, 1L)
  m <- modules[[1L]]
  expect_setequal(m$contract$returned, "selected")
  # The returned reactive itself is a root, so its closure is in node_ids.
  sel_id <- graph$nodes$id[graph$nodes$type == "reactive" & graph$nodes$name == "selected"]
  expect_true(sel_id %in% m$node_ids)
})

test_that("module_slice handles modules with no outputs and no returns", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "noop_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    NULL",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  modules <- module_slice(graph, tmp)
  expect_length(modules, 1L)
  m <- modules[[1L]]
  expect_equal(m$name, "server")
  expect_length(m$contract$outputs, 0L)
  expect_length(m$contract$returned, 0L)
})

test_that("module_slice extracts a single bare-name return", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "single_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    val <- reactive({ input$x })",
    "    val",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  modules <- module_slice(graph, tmp)
  m <- modules[[1L]]
  expect_setequal(m$contract$returned, "val")
})

test_that("module_slice produces one record per file in a multi-module rhino app", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")

  graph <- build_graph(app_path)
  modules <- module_slice(graph, app_path)

  names <- vapply(modules, function(m) m$name, character(1))
  expect_setequal(names, c("app/main", "app/view/mod_a", "app/view/mod_b"))

  by_name <- setNames(modules, names)
  expect_setequal(by_name[["app/view/mod_a"]]$contract$inputs, "which")
  expect_setequal(by_name[["app/view/mod_b"]]$contract$outputs, "echo")
})
