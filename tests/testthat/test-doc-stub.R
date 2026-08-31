test_that("render_doc_stub produces the canonical sections for a feature", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)
  rendered <- render_doc_stub(features[[1L]], graph)

  expect_match(rendered$text, "^# Feature: doubled")
  expect_match(rendered$text, "## Intended use")
  expect_match(rendered$text, "## Risk classification")
  expect_match(rendered$text, "## Reactive subgraph")
  expect_match(rendered$text, "## Functions called")
  expect_match(rendered$text, "## Packages used")
  expect_match(rendered$text, "## Warnings")
  expect_match(rendered$text, "## Reviewers")
})

test_that("render_doc_stub uses placeholders for undeclared manifest fields", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)
  rendered <- render_doc_stub(features[[1L]], graph)

  expect_match(rendered$text, "(not declared", fixed = TRUE)
})

test_that("render_doc_stub embeds manifest text when present", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(
      name = "doubled",
      intended_use = "Display the doubled value to the user.",
      risk_classification = "low",
      roots = list(list(output = "doubled"))
    )),
    modules = list()
  )
  features <- apply_manifest(manifest, graph)
  rendered <- render_doc_stub(features[[1L]], graph)

  expect_match(rendered$text, "Display the doubled value", fixed = TRUE)
  expect_match(rendered$text, "low\n", fixed = TRUE)
})

test_that("render_doc_stub for a module includes the contract section", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$step })",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  modules <- module_slice(graph, tmp)
  rendered <- render_doc_stub(modules[[1L]], graph)

  expect_match(rendered$text, "^# Module: server")
  expect_match(rendered$text, "## Module contract")
  expect_match(rendered$text, "Inputs: step", fixed = TRUE)
  expect_match(rendered$text, "Outputs: count", fixed = TRUE)
})

test_that("merge_doc_stub preserves a hand-edited Reviewers section", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)
  rendered <- render_doc_stub(features[[1L]], graph)

  custom_existing <- gsub(
    "- Developer: __________________ Date: __________",
    "- Developer: Vedha Date: 2026-04-28",
    rendered$text, fixed = TRUE
  )

  merged <- merge_doc_stub(rendered, custom_existing)
  expect_match(merged, "Vedha Date: 2026-04-28", fixed = TRUE)
})

test_that("merge_doc_stub refreshes auto-filled sections", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)
  rendered <- render_doc_stub(features[[1L]], graph)

  # Stale on-disk text where the Warnings section lists an obsolete code.
  stale <- gsub("(none)", "- SVT-W999 — fictional warning",
                rendered$text, fixed = TRUE)
  merged <- merge_doc_stub(rendered, stale)
  expect_false(grepl("SVT-W999", merged, fixed = TRUE))
})

test_that("write_doc_stub creates a file and merges on second pass", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  path <- write_doc_stub(features[[1L]], graph, out_dir)
  expect_true(file.exists(path))

  # Edit the file, simulate a developer signing off.
  txt <- paste(readLines(path), collapse = "\n")
  edited <- sub(
    "- Developer: __________________ Date: __________",
    "- Developer: Vedha Date: 2026-04-28",
    txt, fixed = TRUE
  )
  writeLines(edited, path)

  # Regenerate — the signature should survive.
  write_doc_stub(features[[1L]], graph, out_dir)
  txt2 <- paste(readLines(path), collapse = "\n")
  expect_match(txt2, "Vedha Date: 2026-04-28", fixed = TRUE)
})

test_that("render_doc_stub embeds the inventory tables when supplied", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(tibble)",
    "function(input, output, session) {",
    "  output$z <- shiny::renderText({ tibble::tibble(x = 1) })",
    "}"
  ), file.path(tmp, "app.R"))

  graph <- build_graph(tmp)
  features <- default_slice(graph)
  inv_all <- build_inventory(graph, features, tmp)
  feat <- features[[1L]]
  rendered <- render_doc_stub(feat, graph,
                              warnings_in_subgraph = character(),
                              inventory = inv_all[[feat$name]])

  expect_match(rendered$text, "| Package | Function |", fixed = TRUE)
  expect_match(rendered$text, "| Package | Functions |", fixed = TRUE)
  expect_false(grepl("populated by inventory", rendered$text, fixed = TRUE))
})

test_that("render_doc_stub falls back to (none) when inventory is empty", {
  feat <- list(name = "f", kind = "feature", node_ids = character())
  empty_inv <- list(
    feature = "f", kind = "feature",
    functions = empty_function_calls(),
    packages = tibble::tibble(
      package = character(), functions = list(), direct = logical(),
      category = character(), call_count = integer()
    ),
    package_versions = character(),
    warnings = empty_warnings()
  )
  graph <- list(nodes = tibble::tibble(
    id = character(), type = character(), name = character(),
    namespace = character(), file = character(),
    line = integer(), col = integer(), warnings = list()
  ), warnings = empty_warnings())
  rendered <- render_doc_stub(feat, graph, inventory = empty_inv)
  fn_chunk <- sub(".*## Functions called\\n([^#]*).*", "\\1", rendered$text)
  expect_match(fn_chunk, "(none)", fixed = TRUE)
})

test_that("render_doc_stub merges inventory warning codes with subgraph codes", {
  feat <- list(name = "f", kind = "feature", node_ids = character())
  inv <- list(
    feature = "f", kind = "feature",
    functions = empty_function_calls(),
    packages = tibble::tibble(
      package = character(), functions = list(), direct = logical(),
      category = character(), call_count = integer()
    ),
    package_versions = character(),
    warnings = tibble::tibble(
      code = "SVT-W205", file = NA_character_,
      line = NA_integer_, col = NA_integer_,
      message = warning_message("SVT-W205")
    )
  )
  graph <- list(nodes = tibble::tibble(
    id = character(), type = character(), name = character(),
    namespace = character(), file = character(),
    line = integer(), col = integer(), warnings = list()
  ), warnings = empty_warnings())

  rendered <- render_doc_stub(feat, graph,
                              warnings_in_subgraph = "SVT-W010",
                              inventory = inv)
  expect_match(rendered$text, "SVT-W010", fixed = TRUE)
  expect_match(rendered$text, "SVT-W205", fixed = TRUE)
})

test_that("warnings_in_subgraph attributes warnings via node-list-column codes", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$x <- renderText('a')",
    "  output$x <- renderText('b')",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  features <- default_slice(graph)
  feat_x <- Filter(function(f) f$name == "x", features)[[1L]]
  hits <- warnings_in_subgraph(feat_x, graph)
  expect_true("SVT-W010" %in% hits)
})

test_that("a subgraph warning is attributed by node_id, not by line", {
  graph <- svt_build_graph(svt_parse(fixture_path("line_collision")))
  feats <- svt_slice(graph)
  by_name <- setNames(feats$records, vapply(feats$records, function(r) r$name,
                                            character(1)))

  # Both outputs are defined on the same source line, so a (file, line)
  # match would hand SVT-W001 to `plain` as well.
  nodes <- graph$nodes[graph$nodes$type == "output", , drop = FALSE]
  expect_equal(length(unique(nodes$line)), 1L)

  expect_true("SVT-W001" %in% warnings_in_subgraph(by_name[["dynamic"]], graph))
  expect_false("SVT-W001" %in% warnings_in_subgraph(by_name[["plain"]], graph))
})

test_that("file-level warnings are not attributed to a feature", {
  # SVT-W004 (conditional source()) describes how the app brings code into
  # scope, not what any feature does. It carries no node_id, and guessing
  # an owner by line number would be coincidence dressed as attribution.
  graph <- svt_build_graph(svt_parse(fixture_path("traditional_with_source")))
  expect_true("SVT-W004" %in% graph$warnings$code)
  expect_true(all(is.na(graph$warnings$node_id[graph$warnings$code == "SVT-W004"])))

  feats <- svt_slice(graph)
  seen <- unlist(lapply(feats$records, warnings_in_subgraph, graph = graph))
  expect_false("SVT-W004" %in% seen)
})

test_that("a file-level warning still reaches the app-wide artifacts", {
  # Dropping it from the per-feature stubs must not drop it from the packet:
  # the index and the signed report are where app-level codes belong.
  path <- fixture_path("traditional_with_source")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  inv <- svt_inventory(feats$graph, feats)

  index <- render_index_md(feats, inv, feats$graph, path)$text
  expect_match(index, "SVT-W004", fixed = TRUE)

  report <- render_validation_report(feats, inv, feats$graph, path)$text
  expect_match(report, "SVT-W004", fixed = TRUE)
})
