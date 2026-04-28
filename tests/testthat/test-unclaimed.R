test_that("detect_unclaimed_outputs returns empty when no manifest is supplied", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  unclaimed <- detect_unclaimed_outputs(graph, empty_manifest(),
                                        manifest_supplied = FALSE)
  expect_equal(nrow(unclaimed), 0L)
})

test_that("detect_unclaimed_outputs flags an output not claimed by the manifest", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ input$x })",
    "  output$b <- renderText({ input$y })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  manifest <- list(
    features = list(list(name = "feat_a", intended_use = "...",
                         risk_classification = "low",
                         roots = list(list(output = "a")))),
    modules = list()
  )
  unclaimed <- detect_unclaimed_outputs(graph, manifest, manifest_supplied = TRUE)
  expect_equal(nrow(unclaimed), 1L)
  expect_true(grepl("output:b", unclaimed$message[1L], fixed = TRUE))
  expect_equal(unclaimed$code[1L], "SVT-W103")
})

test_that("detect_unclaimed_outputs is empty when every output is claimed", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(name = "f", intended_use = "...",
                         risk_classification = "low",
                         roots = list(list(output = "doubled")))),
    modules = list()
  )
  unclaimed <- detect_unclaimed_outputs(graph, manifest, manifest_supplied = TRUE)
  expect_equal(nrow(unclaimed), 0L)
})

test_that("detect_unclaimed_outputs ignores module-internal outputs", {
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
  manifest <- list(
    features = list(list(name = "feat_msg", intended_use = "...",
                         risk_classification = "low",
                         roots = list(list(output = "msg")))),
    modules = list()
  )
  unclaimed <- detect_unclaimed_outputs(graph, manifest, manifest_supplied = TRUE)
  # Module-internal `count` (namespaced under counter_server) is not a
  # top-level output → not flagged.
  expect_equal(nrow(unclaimed), 0L)
})

test_that("detect_orphan_module_instances returns empty when wrappers resolve", {
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
  orphans <- detect_orphan_module_instances(graph, tmp)
  expect_equal(nrow(orphans), 0L)
})
