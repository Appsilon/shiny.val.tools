coverage_for <- function(app, manifest = NULL, results = NULL,
                         lenient = FALSE) {
  with_svt_cache({
    parsed <- svt_parse(fixture_path(app))
    graph <- svt_build_graph(parsed)
    feats <- svt_slice(graph, manifest = manifest, lenient = lenient)
    inv <- svt_inventory(graph, feats)
    surf <- svt_test_surface(feats, inv)
    svt_test_coverage(surf, feats, results = results)
  })
}

test_that("an annotated test covers its feature", {
  cov <- coverage_for("traditional_tested")
  e <- cov$entries[["doubled"]]

  expect_equal(e$status, "covered")
  expect_equal(nrow(e$tests), 1L)
  expect_equal(e$tests$link, "annotation")
  expect_equal(e$warnings, character())
})

test_that("an unannotated testServer block infers its feature from outputs", {
  cov <- coverage_for("traditional_tested")
  e <- cov$entries[["scaled"]]

  expect_equal(e$status, "covered")
  expect_equal(e$tests$link, "inference")
})

test_that("a feature with no mapped test is uncovered and raises W301", {
  cov <- coverage_for("traditional_tested")
  e <- cov$entries[["notes"]]

  expect_equal(e$status, "uncovered")
  expect_equal(nrow(e$tests), 0L)
  expect_true("SVT-W301" %in% e$warnings)
})

test_that("a mapped test that exercises nothing observable is partial", {
  cov <- coverage_for("traditional_tested")
  e <- cov$entries[["ratio"]]

  expect_equal(e$status, "partial")
  expect_true("SVT-W302" %in% e$warnings)
  expect_equal(e$unexercised$observables, "ratio")
})

test_that("a mapped test still carrying its skip marker is scaffold", {
  cov <- coverage_for("traditional_tested")
  e <- cov$entries[["summary"]]

  expect_equal(e$status, "scaffold")
  expect_true("SVT-W303" %in% e$warnings)
})

test_that("a test mapping to nothing is an orphan (W304)", {
  cov <- coverage_for("traditional_tested")
  orphans <- cov$tests$file[cov$orphans]

  expect_equal(orphans, "tests/testthat/test-misc.R")
  expect_equal(cov$summary$orphan_tests, 1L)
})

test_that("a box-aliased testServer call resolves to its module", {
  cov <- coverage_for("rhino_tested")
  e <- cov$entries[["app/view/counter"]]

  expect_equal(e$status, "covered")
  expect_equal(e$tests$link, "inference")
})

test_that("verification: not_required with a rationale waives the feature", {
  mf <- list(features = list(list(
    name = "notes", roots = list(list(output = "notes")),
    verification = "not_required",
    rationale_verification = "cosmetic label; no analytical content"
  )))
  cov <- coverage_for("traditional_tested", manifest = mf, lenient = TRUE)
  e <- cov$entries[["notes"]]

  expect_equal(e$status, "waived")
  expect_equal(e$warnings, character())
})

test_that("verification: not_required without a rationale is fatal (W311)", {
  mf <- list(features = list(list(
    name = "notes", roots = list(list(output = "notes")),
    verification = "not_required"
  )))
  expect_error(coverage_for("traditional_tested", manifest = mf),
               "SVT-W311")
})

test_that("a manifest tests: entry credits evidence this tool cannot read", {
  mf <- list(features = list(list(
    name = "notes", roots = list(list(output = "notes")),
    tests = list(list(external = "UAT-014", note = "manual acceptance"))
  )))
  cov <- coverage_for("traditional_tested", manifest = mf, lenient = TRUE)
  e <- cov$entries[["notes"]]

  expect_equal(e$status, "covered")
  expect_equal(e$tests$link, "manifest")
  expect_equal(e$tests$harness, "external")
})

test_that("a manifest tests: file that does not exist raises W305", {
  mf <- list(features = list(list(
    name = "notes", roots = list(list(output = "notes")),
    tests = list(list(file = "tests/cypress/e2e/absent.cy.js"))
  )))
  cov <- coverage_for("traditional_tested", manifest = mf, lenient = TRUE)
  expect_true("SVT-W305" %in% cov$entries[["notes"]]$warnings)
})

test_that("an @covers naming an unknown target is recorded, not silently lost", {
  app <- withr::local_tempdir()
  file.copy(list.files(fixture_path("traditional_tested"), full.names = TRUE),
            app, recursive = TRUE)
  writeLines(c("# @covers feature: renamed_away", "",
               "test_that(\"stale annotation\", {",
               "  expect_true(TRUE)", "})"),
             file.path(app, "tests", "testthat", "test-stale.R"))

  cov <- with_svt_cache({
    parsed <- svt_parse(app)
    graph <- svt_build_graph(parsed)
    feats <- svt_slice(graph)
    surf <- svt_test_surface(feats, svt_inventory(graph, feats))
    svt_test_coverage(surf, feats)
  })

  expect_true(length(cov$unknown) >= 1L)
  expect_true("feature:renamed_away" %in%
                unlist(cov$tests$unknown_targets[cov$unknown]))
})

test_that("ingested JUnit results are stamped onto every mapped test", {
  xml <- withr::local_tempfile(fileext = ".xml")
  writeLines(c(
    "<testsuites><testsuite name=\"a\">",
    "<testcase classname=\"t\" name=\"doubled reflects the input\"/>",
    "<testcase classname=\"t\" name=\"scaled uses the default factor\">",
    "<failure message=\"boom\"/></testcase>",
    "</testsuite></testsuites>"), xml)

  cov <- coverage_for("traditional_tested", results = xml)

  expect_equal(cov$entries[["doubled"]]$tests$result, "pass")
  expect_equal(cov$entries[["scaled"]]$tests$result, "fail")
  expect_true("SVT-W310" %in% cov$entries[["scaled"]]$warnings)
  # A mapped test the report never mentions is a test CI did not run,
  # which is as much a finding as a failure.
  expect_equal(cov$entries[["ratio"]]$tests$result, "missing")
  expect_true("SVT-W310" %in% cov$entries[["ratio"]]$warnings)
})

test_that("strict_verification aborts on a high-risk uncovered feature", {
  mf <- list(features = list(list(
    name = "notes", roots = list(list(output = "notes")),
    risk_classification = "high"
  )))
  expect_error(
    svt_validate(fixture_path("traditional_tested"), manifest = mf,
                 out_dir = withr::local_tempdir(), lenient = TRUE,
                 strict_verification = TRUE),
    "SVT-W301"
  )
})

test_that("tests = 'coverage' is the default when a test tree exists", {
  expect_equal(resolve_tests_depth(NULL,
                                   file.path(fixture_path("traditional_tested"),
                                             "tests")),
               "coverage")
  expect_equal(resolve_tests_depth(NULL,
                                   file.path(fixture_path("traditional_basic"),
                                             "tests")),
               "surface")
  expect_equal(resolve_tests_depth("off", NULL), "off")
})
