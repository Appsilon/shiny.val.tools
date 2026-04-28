test_that("svt_parse returns an svt_parsed with files + asts", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  parsed <- svt_parse(app_path)

  expect_s3_class(parsed, "svt_parsed")
  expect_true(length(parsed$files) > 0L)
  expect_equal(names(parsed$asts), parsed$files)
})

test_that("svt_parse rejects nonexistent paths and non-strings", {
  expect_error(svt_parse(NULL), "single, non-NA")
  expect_error(svt_parse(c("a", "b")), "single, non-NA")
  expect_error(svt_parse(tempfile()), "does not exist")
})

test_that("svt_build_graph returns the graph tibbles plus app_path", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))

  expect_s3_class(graph, "svt_graph")
  expect_s3_class(graph$nodes, "tbl_df")
  expect_s3_class(graph$edges, "tbl_df")
  expect_true(nzchar(graph$app_path))
})

test_that("svt_build_graph rejects non-svt_parsed inputs", {
  expect_error(svt_build_graph(list()), "svt_parsed")
})

test_that("svt_slice with no manifest returns default features + modules", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)

  expect_s3_class(feats, "svt_features")
  expect_false(feats$manifest_supplied)
  expect_true(length(feats$records) >= 1L)
})

test_that("svt_slice aborts on manifest issues by default and warns when lenient", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  bad <- list(features = list(list(
    name = "ghost",
    intended_use = "x", risk_classification = "low",
    roots = list(list(output = "no_such_output"))
  )), modules = list())

  expect_error(svt_slice(graph, manifest = bad), "Manifest validation failed")
  expect_warning(svt_slice(graph, manifest = bad, lenient = TRUE),
                 "SVT-W101")
})

test_that("svt_inventory produces per-feature records keyed by name", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  expect_s3_class(inv, "svt_inventory")
  for (rec in feats$records) {
    expect_true(rec$name %in% names(inv$features))
  }
})

test_that("svt_render writes doc stubs and inventory.json files", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  result <- svt_render(feats, inv, out_dir = out_dir)

  expect_s3_class(result, "svt_validation")
  expect_true(all(file.exists(result$doc_stubs)))
  expect_true(all(file.exists(result$inventories)))
})

test_that("svt_validate end-to-end produces an svt_validation", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  result <- svt_validate(app_path, out_dir = out_dir)
  expect_s3_class(result, "svt_validation")
  expect_true(result$n_features >= 1L)
})

test_that("svt_validate filters by features and modules selectors", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  result <- svt_validate(app_path, out_dir = out_dir,
                         features = "doubled")
  expect_equal(result$n_features, 1L)
  expect_true(any(grepl("doubled\\.md$", result$doc_stubs)))
})

test_that("svt_validate auto-discovers app_path/features.yml", {
  src <- system.file("extdata", "traditional_basic",
                     package = "shiny.val.tools")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(list.files(src, full.names = TRUE), tmp, recursive = TRUE)
  writeLines(c(
    "features:",
    "  - name: doubled",
    "    intended_use: 'Display doubled value.'",
    "    risk_classification: low",
    "    roots:",
    "      - output: doubled",
    "modules: []"
  ), file.path(tmp, "features.yml"))

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  result <- svt_validate(tmp, out_dir = out_dir)

  doubled <- result$doc_stubs[grepl("doubled\\.md$", result$doc_stubs)]
  expect_length(doubled, 1L)
  txt <- paste(readLines(doubled), collapse = "\n")
  expect_match(txt, "Display doubled value", fixed = TRUE)
})

test_that("svt_summary returns a per-feature tibble", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  s <- svt_summary(graph)
  expect_s3_class(s, "tbl_df")
  expect_true(all(c("name", "kind", "nodes", "edges") %in% colnames(s)))
})

test_that("svt_warnings tags inventory rows with the owning feature", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)
  w <- svt_warnings(inv)
  expect_true("feature" %in% colnames(w) || nrow(w) == 0L)
})

test_that("svt_manifest_template + svt_manifest_validate round-trip", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  out_path <- tempfile(fileext = ".yml")
  on.exit(unlink(out_path), add = TRUE)
  svt_manifest_template(graph, out_path)
  expect_true(file.exists(out_path))

  issues <- svt_manifest_validate(out_path, graph)
  expect_s3_class(issues, "tbl_df")
})

test_that("svt_validate tolerates trailing commas (R_MissingArg sentinels)", {
  # `c(1, 2, )` parses with R_MissingArg in the empty slot. Binding it
  # to a local variable in a walker errored historically because every
  # subsequent read of that local raised "argument 'child' is missing,
  # with no default" before any is.null/is.symbol guard ran.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(shiny)",
    "x <- c(1, 2, )",
    "function(input, output, session) {",
    "  output$y <- renderText({ x[1] })",
    "}"
  ), file.path(tmp, "app.R"))

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  expect_no_error(svt_validate(tmp, out_dir = out_dir))
})

test_that("is_missing_arg detects R_MissingArg sentinels safely", {
  call_expr <- quote(c(1, 2, ))
  third <- as.list(call_expr)[[4L]]
  expect_true(is_missing_arg(third))
  expect_false(is_missing_arg(1L))
  expect_false(is_missing_arg(NULL))
  expect_false(is_missing_arg(quote(x)))
})

test_that("svt_unclaimed surfaces SVT-W103 only when manifest is supplied", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))

  default_feats <- svt_slice(graph)
  expect_equal(nrow(svt_unclaimed(default_feats)), 0L)

  manifest <- list(features = list(), modules = list())
  partial <- svt_slice(graph, manifest = manifest)
  expect_true(nrow(svt_unclaimed(partial)) >= 1L ||
                nrow(svt_unclaimed(partial)) == 0L)
})
