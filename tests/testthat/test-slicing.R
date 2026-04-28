test_that("upstream_closure follows edges from a root", {
  nodes <- tibble::tibble(
    id = c("output:::a", "reactive:::r", "input:::x"),
    type = c("output", "reactive", "input"),
    name = c("a", "r", "x"),
    namespace = NA_character_, container = NA_character_,
    fq_name = c("a", "r", "x"),
    file = "server.R", line = 1L, col = 1L,
    warnings = list(character(), character(), character())
  )
  edges <- tibble::tibble(
    source_id = c("output:::a", "reactive:::r"),
    target_id = c("reactive:::r", "input:::x"),
    file = "server.R", line = 1L, col = 1L
  )
  closure <- upstream_closure("output:::a", nodes, edges)
  expect_setequal(closure, c("output:::a", "reactive:::r", "input:::x"))
})

test_that("upstream_closure treats a root with no outgoing edges as singleton", {
  nodes <- tibble::tibble(
    id = "output:::a", type = "output", name = "a",
    namespace = NA_character_, container = NA_character_,
    fq_name = "a", file = "server.R", line = 1L, col = 1L,
    warnings = list(character())
  )
  edges <- tibble::tibble(
    source_id = character(), target_id = character(),
    file = character(), line = integer(), col = integer()
  )
  closure <- upstream_closure("output:::a", nodes, edges)
  expect_equal(closure, "output:::a")
})

test_that("default_feature_roots picks up top-level outputs and observers only", {
  nodes <- tibble::tibble(
    id = c("output:::doubled", "output:counter:::count",
           "observer:::download_csv", "input:::x"),
    type = c("output", "output", "observer", "input"),
    name = c("doubled", "count", "download_csv", "x"),
    namespace = c(NA, "counter", NA, NA),
    container = NA_character_,
    fq_name = c("doubled", "counter/count", "download_csv", "x"),
    file = "server.R", line = 1L, col = 1L,
    warnings = list(character(), character(), character(), character())
  )
  roots <- default_feature_roots(nodes)
  expect_setequal(roots$name, c("doubled", "download_csv"))
  expect_false("count" %in% roots$name)
})

test_that("default_slice produces one feature per top-level output (traditional_basic)", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)

  expect_length(features, 1L)
  feat <- features[[1L]]
  expect_equal(feat$name, "doubled")
  expect_equal(feat$kind, "feature")
  expect_length(feat$roots, 1L)
  # Closure includes the output node and the input it reads.
  in_x <- graph$nodes$id[graph$nodes$type == "input" & graph$nodes$name == "x"]
  expect_true(in_x %in% feat$node_ids)
})

test_that("default_slice closure stops at module_instance nodes", {
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
    "  output$msg <- renderText({ input$x })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  features <- default_slice(graph)

  msg_feat <- Filter(function(f) f$name == "msg", features)[[1L]]
  in_x <- graph$nodes$id[graph$nodes$type == "input" & graph$nodes$name == "x" &
                           is.na(graph$nodes$namespace)]
  expect_true(in_x %in% msg_feat$node_ids)
  # The module-internal output `count` is namespaced under counter_server,
  # not a feature root, and is not part of the top-level msg subgraph.
  count_id <- graph$nodes$id[graph$nodes$type == "output" & graph$nodes$name == "count"]
  expect_false(count_id %in% msg_feat$node_ids)
})

test_that("default_slice has no features when the app has no top-level outputs", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines("function(input, output, session) {}", file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  features <- default_slice(graph)
  expect_length(features, 0L)
})

test_that("build_graph returns the canonical six-element list shape", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  g <- build_graph(app_path)
  expect_named(g, c("files", "imports", "sources", "nodes", "edges", "warnings"),
               ignore.order = TRUE)
  expect_s3_class(g$nodes, "tbl_df")
  expect_s3_class(g$edges, "tbl_df")
})
