test_that("build_edges_table returns a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  edges <- build_edges_table(app_path)

  expect_s3_class(edges, "tbl_df")
  expect_named(
    edges,
    c("source_id", "target_id", "file", "line", "col"),
    ignore.order = TRUE
  )
})

test_that("traditional_basic emits one edge: output(doubled) -> input(x)", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  nodes <- build_nodes_table(app_path)
  edges <- build_edges_table(app_path)

  expect_equal(nrow(edges), 1L)
  out_id <- nodes$id[nodes$type == "output" & nodes$name == "doubled"]
  in_id  <- nodes$id[nodes$type == "input"  & nodes$name == "x"]
  expect_equal(edges$source_id, out_id)
  expect_equal(edges$target_id, in_id)
})

test_that("named-reactive reads create an edge from the consumer to the reactive", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  total <- reactive({ input$x })",
    "  output$z <- renderText({ total() + 1 })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  edges <- build_edges_table(tmp)

  total_id <- nodes$id[nodes$type == "reactive" & nodes$name == "total"]
  out_z    <- nodes$id[nodes$type == "output"   & nodes$name == "z"]
  in_x     <- nodes$id[nodes$type == "input"    & nodes$name == "x"]

  expect_true(any(edges$source_id == out_z & edges$target_id == total_id))
  expect_true(any(edges$source_id == total_id & edges$target_id == in_x))
})

test_that("multiple reads of the same name collapse into one edge (first source_loc wins)", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$z <- renderText({ input$x + input$x + input$x })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  edges <- build_edges_table(tmp)

  out_z <- nodes$id[nodes$type == "output" & nodes$name == "z"]
  in_x  <- nodes$id[nodes$type == "input"  & nodes$name == "x"]

  edge_row <- edges[edges$source_id == out_z & edges$target_id == in_x, ]
  expect_equal(nrow(edge_row), 1L)
})

test_that("references outside any definition do not produce edges", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "library(shiny)",
    "x_global <- 42",
    "format <- function(z) z",
    "function(input, output, session) {",
    "  output$z <- renderText({ format(x_global) })",
    "}"
  ), file.path(tmp, "server.R"))

  edges <- build_edges_table(tmp)
  nodes <- build_nodes_table(tmp)

  # No edges from `format()` or `x_global` reads; only output-z exists,
  # and it has no Shiny dependencies (`format` is not a defined node).
  expect_equal(nrow(edges), 0L)
})

test_that("rv$x reads create an edge to the value node", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  rv <- reactiveValues(a = 0)",
    "  output$z <- renderText({ rv$a })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  edges <- build_edges_table(tmp)

  out_z <- nodes$id[nodes$type == "output" & nodes$name == "z"]
  v_a   <- nodes$id[nodes$type == "value"  & nodes$name == "a"]

  expect_true(any(edges$source_id == out_z & edges$target_id == v_a))
})
