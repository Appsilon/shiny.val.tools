fixture_pkg <- function() {
  path <- system.file("extdata", "modulepkg_basic", package = "shiny.val.tools")
  expect_true(nzchar(path), info = "fixture missing from inst/extdata/")
  path
}

test_that("is_package_target detects a source package by DESCRIPTION + NAMESPACE", {
  expect_true(is_package_target(fixture_pkg()))

  app <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  expect_false(is_package_target(app))
})

test_that("package_exports reads the NAMESPACE export list", {
  exports <- package_exports(fixture_pkg())

  expect_setequal(exports, c("counter_server", "counter_ui"))
  # internal module functions are deliberately absent
  expect_false("readout_server" %in% exports)
})

test_that("package_exports returns empty for a non-package path", {
  app <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  expect_identical(package_exports(app), character())
})

test_that("exported_modules pairs each exported server with its ui function", {
  mods <- exported_modules(fixture_pkg())

  expect_s3_class(mods, "tbl_df")
  expect_equal(nrow(mods), 1L)
  expect_equal(mods$server_fn, "counter_server")
  expect_equal(mods$ui_fn, "counter_ui")
  # module identity stays the graph's file-derived coordinate
  expect_equal(mods$module, "R/counter_mod")
})

test_that("slicing a package target keeps only exported modules", {
  path <- fixture_pkg()
  graph <- svt_build_graph(svt_parse(path))
  sliced <- svt_slice(graph)

  names_out <- vapply(sliced$records, function(r) r$name, character(1))
  expect_equal(names_out, "counter_server")
})

test_that("an unexported child module still appears inside its exported parent", {
  path <- fixture_pkg()
  graph <- svt_build_graph(svt_parse(path))
  sliced <- svt_slice(graph)

  rec <- sliced$records[[1]]
  types <- graph$nodes$type[graph$nodes$id %in% rec$node_ids]
  expect_true("module_instance" %in% types)
})

test_that("app targets are unaffected by exported-module filtering", {
  path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(path))
  sliced <- svt_slice(graph)

  expect_gt(length(sliced$records), 0)
})

test_that("an exported module record keeps its graph identity in `module`", {
  path <- fixture_path("modulepkg_basic")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  rec <- Filter(function(r) identical(r$kind, "module"), feats$records)[[1]]

  # Named by what consumers call...
  expect_equal(rec$name, "counter_server")
  # ...but still joinable to the namespace the graph's nodes carry.
  expect_equal(rec$module, "R/counter_mod")

  ns <- feats$graph$nodes$namespace
  expect_true(rec$module %in% ns[!is.na(ns)])
})

test_that("an app-target module record has no package-target fields", {
  path <- fixture_path("traditional_module")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  rec <- Filter(function(r) identical(r$kind, "module"), feats$records)[[1]]

  # `module` is the package-target join key; an app module is already named
  # by its graph identity, so there is nothing to bridge.
  expect_null(rec$module)
  expect_null(rec$server_fn)
})
