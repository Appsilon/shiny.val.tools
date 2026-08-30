fixture <- function(name) fixture_path(name)

surface_for <- function(app, manifest = NULL) {
  with_svt_cache({
    parsed <- svt_parse(fixture(app))
    graph <- svt_build_graph(parsed)
    feats <- svt_slice(graph, manifest = manifest)
    inv <- svt_inventory(graph, feats)
    svt_test_surface(feats, inv)
  })
}

test_that("a feature surface names its stimuli, observables and helpers", {
  surf <- surface_for("traditional_basic")
  s <- surf$surfaces[["doubled"]]

  expect_equal(s$name, "doubled")
  expect_equal(s$kind, "feature")
  expect_equal(s$stimuli$name, "x")
  expect_equal(s$observables$name, "doubled")
  expect_equal(s$observables$def_call, "renderText")
  expect_true("double_it" %in% s$helpers$fn)
  expect_true("R/helpers.R" %in% s$helpers$file)
})

test_that("each stimulus records the roots it drives", {
  surf <- surface_for("dynamic_ui")
  s <- surf$surfaces[["summary"]]

  expect_setequal(s$stimuli$name, c("dataset", "threshold"))
  expect_equal(s$stimuli$drives[[1]], "summary")
  expect_equal(s$stimuli$drives[[2]], "summary")
})

test_that("every surface selects the testserver harness", {
  surf <- surface_for("dynamic_ui")
  expect_true(all(vapply(surf$surfaces, function(s) s$harness, character(1)) ==
                    "testserver"))
})

test_that("a renderUI observable is opaque and raises SVT-W312", {
  surf <- surface_for("dynamic_ui")
  s <- surf$surfaces[["controls"]]

  expect_equal(s$observables$def_call, "renderUI")
  expect_equal(s$observables$observable_via, "opaque")
  expect_true("SVT-W312" %in% s$warnings)
  expect_match(s$harness_reason, "opaque")
})

test_that("a renderText observable is directly observable and raises no W312", {
  surf <- surface_for("traditional_basic")
  s <- surf$surfaces[["doubled"]]

  expect_equal(s$observables$observable_via, "direct")
  expect_false("SVT-W312" %in% s$warnings)
})

test_that("static-analysis limits in the closure become blockers plus SVT-W308", {
  surf <- surface_for("dynamic_ui")
  s <- surf$surfaces[["controls"]]

  expect_equal(s$blockers, "SVT-W002")
  expect_true("SVT-W308" %in% s$warnings)

  # The feature that merely reads the input is not itself blocked.
  expect_equal(surf$surfaces[["summary"]]$blockers, character())
})

test_that("a module surface carries namespace-local stimuli, internals and observables", {
  surf <- surface_for("modulepkg_basic")
  s <- surf$surfaces[["counter_server"]]

  expect_equal(s$kind, "module")
  expect_equal(s$harness, "testserver")
  expect_match(s$harness_reason, "module server function")
  expect_equal(s$stimuli$name, "step")
  expect_equal(s$observables$name, "total")
  expect_equal(s$internals$name, "running")
  # Under testServer() a named reactive is callable by name, so an
  # internal needs no plumbing to be observable.
  expect_equal(s$internals$observable_via, "direct")
})

test_that("module_instance nodes are terminals, not observables, and link their artifact", {
  surf <- surface_for("modulepkg_basic")
  s <- surf$surfaces[["counter_server"]]

  expect_equal(s$terminals$kind, "module_instance")
  expect_equal(s$terminals$name, "R/readout_mod")
  expect_equal(s$terminals$artifact, "R--readout_mod.md")
  expect_false(any(s$terminals$name %in% s$observables$name))
  expect_false(any(s$terminals$name %in% s$internals$name))
})

test_that("helper category comes from the manifest's reserved 'app' key", {
  manifest <- list(
    features = list(list(
      name = "doubled",
      roots = list(list(output = "doubled")),
      package_categories = list(app = "method")
    )),
    modules = list()
  )
  surf <- surface_for("traditional_basic", manifest = manifest)
  s <- surf$surfaces[["doubled"]]

  expect_true(all(s$helpers$category == "method"))
})

test_that("helpers are ordered method first, then by name", {
  helpers <- tibble::tibble(
    fn = c("zeta", "alpha", "beta"),
    category = c("method", "utility", "method")
  )
  expect_equal(order_helpers(helpers)$fn, c("beta", "zeta", "alpha"))
})

test_that("surface_hash is stable across runs and changes with the surface", {
  a <- surface_for("traditional_basic")$surfaces[["doubled"]]
  b <- surface_for("traditional_basic")$surfaces[["doubled"]]
  expect_equal(a$surface_hash, b$surface_hash)

  c <- surface_for("dynamic_ui")$surfaces[["summary"]]
  expect_false(identical(a$surface_hash, c$surface_hash))
})

test_that("surface_hash ignores source location but tracks the testable surface", {
  base <- list(
    stimuli = c("arm", "cutoff"), observables = c("km_plot=renderPlot"),
    internals = "km_fit", helpers = "R/km.R:compute", blockers = character()
  )
  moved <- base
  same <- surface_hash_of(base$stimuli, base$observables, base$internals,
                          base$helpers, base$blockers)
  expect_equal(same, surface_hash_of(rev(moved$stimuli), moved$observables,
                                     moved$internals, moved$helpers,
                                     moved$blockers))
  expect_false(identical(
    same,
    surface_hash_of(c(base$stimuli, "extra"), base$observables, base$internals,
                    base$helpers, base$blockers)
  ))
})

test_that("svt_test_surface requires the typed inputs", {
  expect_error(svt_test_surface(list(), NULL), "svt_features")
})

test_that("test_surface.json carries the schema version and the derived surface", {
  out <- withr::local_tempdir()
  surf <- surface_for("dynamic_ui")

  path <- write_test_surface_json(surf$surfaces[["controls"]], out)
  expect_true(file.exists(path))
  expect_equal(basename(path), "test_surface.json")
  expect_equal(basename(dirname(path)), "controls")

  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json, '"schema_version":"1.0"')
  expect_match(json, '"harness":"testserver"')
  expect_match(json, '"observable_via":"opaque"')
  expect_match(json, '"blockers":\\["SVT-W002"\\]')
})

test_that("test_surface.json is byte-identical across runs", {
  out1 <- withr::local_tempdir()
  out2 <- withr::local_tempdir()
  s <- surface_for("traditional_basic")$surfaces[["doubled"]]

  p1 <- write_test_surface_json(s, out1)
  p2 <- write_test_surface_json(s, out2)
  expect_equal(unname(tools::md5sum(p1)), unname(tools::md5sum(p2)))
})

test_that("the doc stub gains a Test surface section when a surface is supplied", {
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture("dynamic_ui")))
    feats <- svt_slice(graph)
    inv <- svt_inventory(graph, feats)
    surf <- svt_test_surface(feats, inv)
    rec <- Filter(function(r) r$name == "controls", feats$records)[[1]]

    with_surface <- render_doc_stub(rec, graph, character(),
                                    inventory = inv$features[["controls"]],
                                    surface = surf$surfaces[["controls"]])
    expect_true("Test surface" %in% with_surface$auto_keys)
    expect_match(with_surface$text, "## Test surface")
    expect_match(with_surface$text, "renderUI")
    expect_match(with_surface$text, "SVT-W312")

    without <- render_doc_stub(rec, graph, character(),
                               inventory = inv$features[["controls"]])
    expect_false("Test surface" %in% without$auto_keys)
    expect_false(grepl("## Test surface", without$text, fixed = TRUE))
  })
})

test_that("svt_render writes test_surface.json and tracks it in the manifest", {
  out <- withr::local_tempdir()
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture("traditional_basic")))
    feats <- svt_slice(graph)
    inv <- svt_inventory(graph, feats)
    surf <- svt_test_surface(feats, inv)

    res <- svt_render(feats, inv, out_dir = out, surface = surf)

    expect_true(file.exists(file.path(out, "doubled", "test_surface.json")))
    expect_true(length(res$test_surfaces) > 0)

    tracked <- read_artifact_manifest(out)$path
    expect_true("doubled/test_surface.json" %in% tracked)

    stub <- paste(readLines(file.path(out, "doubled.md"), warn = FALSE),
                  collapse = "\n")
    expect_match(stub, "## Test surface")
  })
})

test_that("svt_render without a surface writes no surface artifacts", {
  out <- withr::local_tempdir()
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture("traditional_basic")))
    feats <- svt_slice(graph)
    inv <- svt_inventory(graph, feats)

    res <- svt_render(feats, inv, out_dir = out)

    expect_false(file.exists(file.path(out, "doubled", "test_surface.json")))
    expect_length(res$test_surfaces, 0)
    stub <- paste(readLines(file.path(out, "doubled.md"), warn = FALSE),
                  collapse = "\n")
    expect_false(grepl("## Test surface", stub, fixed = TRUE))
  })
})

test_that("svt_validate derives the surface by default and can be turned off", {
  out <- withr::local_tempdir()
  res <- svt_validate(fixture("traditional_basic"), out_dir = out)
  expect_true(file.exists(file.path(out, "doubled", "test_surface.json")))

  out2 <- withr::local_tempdir()
  res2 <- svt_validate(fixture("traditional_basic"), out_dir = out2,
                       tests = "off")
  expect_false(file.exists(file.path(out2, "doubled", "test_surface.json")))
  expect_length(res2$test_surfaces, 0)
})

test_that("a rendered surface section survives a regeneration with edits elsewhere", {
  out <- withr::local_tempdir()
  with_svt_cache({
    graph <- svt_build_graph(svt_parse(fixture("traditional_basic")))
    feats <- svt_slice(graph)
    inv <- svt_inventory(graph, feats)
    surf <- svt_test_surface(feats, inv)
    svt_render(feats, inv, out_dir = out, surface = surf)

    stub_path <- file.path(out, "doubled.md")
    text <- paste(readLines(stub_path, warn = FALSE), collapse = "\n")
    writeLines(sub("- Developer: __________________ Date: __________",
                   "- Developer: A. Person Date: 2026-01-01", text, fixed = TRUE),
               stub_path)

    svt_render(feats, inv, out_dir = out, surface = surf)
    after <- paste(readLines(stub_path, warn = FALSE), collapse = "\n")
    expect_match(after, "A. Person", fixed = TRUE)
    expect_match(after, "## Test surface")
  })
})
