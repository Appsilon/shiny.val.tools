test_that("build_nodes_table returns a tibble with the documented columns", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  nodes <- build_nodes_table(app_path)

  expect_s3_class(nodes, "tbl_df")
  expect_named(
    nodes,
    c("id", "type", "name", "namespace", "container", "fq_name",
      "file", "line", "col", "warnings"),
    ignore.order = TRUE
  )
})

test_that("traditional_basic produces one input node and one output node", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  nodes <- build_nodes_table(app_path)

  expect_setequal(nodes$type, c("input", "output"))
  in_row <- nodes[nodes$type == "input", ]
  out_row <- nodes[nodes$type == "output", ]
  expect_equal(in_row$name, "x")
  expect_equal(out_row$name, "doubled")
})

test_that("build_nodes_table assigns stable, content-addressed ids", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")

  n1 <- build_nodes_table(app_path)
  n2 <- build_nodes_table(app_path)

  expect_identical(sort(n1$id), sort(n2$id))
  # IDs are unique within a run.
  expect_equal(length(unique(n1$id)), nrow(n1))
})

test_that("build_nodes_table dedupes output nodes by (namespace, name)", {
  # Two assignments to the same output → one canonical node + an SVT-W010
  # warning attached to it.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$x <- renderText('a')",
    "  output$x <- renderText('b')",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  out_rows <- nodes[nodes$type == "output", ]
  expect_equal(nrow(out_rows), 1L)
  expect_true("SVT-W010" %in% out_rows$warnings[[1]])
})

test_that("build_nodes_table emits one value node per (namespace, container, name)", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  rv <- reactiveValues(a = 0)",
    "  observe({ rv$a <- 1 })",
    "  observe({ rv$a <- 2 })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  values <- nodes[nodes$type == "value", ]
  expect_equal(nrow(values), 1L)
  expect_equal(values$name, "a")
  expect_equal(values$container, "rv")
})

test_that("build_nodes_table namespaces module-defined nodes", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$x })",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)

  out_row <- nodes[nodes$type == "output" & nodes$name == "count", ]
  expect_equal(nrow(out_row), 1L)
  expect_equal(out_row$namespace, "server")
  expect_equal(out_row$fq_name, "server/count")

  in_row <- nodes[nodes$type == "input" & nodes$name == "x", ]
  expect_equal(nrow(in_row), 1L)
  expect_equal(in_row$namespace, "server")
})

test_that("build_nodes_table emits one input node per (namespace, name) regardless of read count", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ input$x + input$x })",
    "  output$b <- renderText({ input$x })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  ins <- nodes[nodes$type == "input", ]
  expect_equal(nrow(ins), 1L)
  expect_equal(ins$name, "x")
})

test_that("top-level function definitions never become graph nodes", {
  app <- fixture_path("traditional_basic")

  defs <- with_svt_cache(build_definitions_table(app))
  nodes <- with_svt_cache(build_nodes_table(app))

  # The definitions table knows about the helpers ...
  expect_true("double_it" %in% defs$name[defs$kind == "function"])
  # ... and the graph does not.
  expect_false("function" %in% nodes$type)
  expect_false("double_it" %in% nodes$name)
})
