report_fixture <- function() {
  path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  inv <- svt_inventory(feats$graph, feats)
  list(features = feats, inventory = inv, graph = feats$graph, app_path = path)
}

render_fixture_report <- function(report_meta = NULL) {
  f <- report_fixture()
  render_validation_report(f$features, f$inventory, f$graph, f$app_path,
                           report_meta = report_meta)
}

test_that("the report renders every specified section in order", {
  rendered <- render_fixture_report()

  headings <- grep("^## ", strsplit(rendered$text, "\n")[[1]], value = TRUE)
  expect_equal(
    sub("^## ", "", headings),
    c("Document control", "System identification", "Scope of analysis",
      "Method and limitations", "Features and risk classification",
      "Verification status", "Analysis findings", "Conclusion", "Approval")
  )
})

test_that("Conclusion and Approval are the only human-owned sections", {
  rendered <- render_fixture_report()

  expect_false("Conclusion" %in% rendered$auto_keys)
  expect_false("Approval" %in% rendered$auto_keys)
  expect_true(all(c("Document control", "Method and limitations",
                    "Analysis findings") %in% rendered$auto_keys))
})

test_that("the report carries no generation timestamp", {
  a <- render_fixture_report()$text
  b <- render_fixture_report()$text

  # Byte-identical across runs — the determinism contract.
  expect_identical(a, b)
  expect_false(grepl("\\d{4}-\\d{2}-\\d{2}T", a))
})

test_that("sponsor fields land in Document control and absent ones are visible", {
  rendered <- render_fixture_report(report_meta = list(
    document_id = "VAL-2026-014",
    sponsor = "Example Sponsor Ltd"
  ))

  expect_match(rendered$text, "VAL-2026-014", fixed = TRUE)
  expect_match(rendered$text, "Example Sponsor Ltd", fixed = TRUE)
  # A field the sponsor did not declare must read as a visible gap.
  expect_match(rendered$text, "(not declared)", fixed = TRUE)
})

test_that("approvers seed the Approval table", {
  rendered <- render_fixture_report(report_meta = list(
    approvers = list(
      list(role = "System Owner", name = "A. Person"),
      list(role = "Quality Assurance")
    )
  ))

  expect_match(rendered$text, "System Owner", fixed = TRUE)
  expect_match(rendered$text, "A. Person", fixed = TRUE)
  expect_match(rendered$text, "Quality Assurance", fixed = TRUE)
})

test_that("verification status is stated, not omitted", {
  rendered <- render_fixture_report()

  expect_match(rendered$text, "Verification status", fixed = TRUE)
  expect_match(rendered$text, "coverage was not assessed", ignore.case = TRUE)
})

test_that("verification status does not deny the shipped test-surface layer", {
  rendered <- render_fixture_report()

  # The signable report must not understate the tool either: surface
  # derivation and scaffolding are implemented, so calling the whole testing
  # layer unimplemented would be a false statement in a signed document.
  expect_false(grepl("not yet implemented", rendered$text, fixed = TRUE))
  expect_match(rendered$text, "test surface", ignore.case = TRUE)
})

test_that("findings report every warning code present with its meaning", {
  f <- report_fixture()
  rendered <- render_validation_report(f$features, f$inventory, f$graph,
                                       f$app_path)

  codes <- unique(svt_warnings(f$inventory)$code)
  for (code in codes) {
    expect_match(rendered$text, code, fixed = TRUE)
  }
})

test_that("write_validation_report preserves human sections on regeneration", {
  out <- withr::local_tempdir()
  f <- report_fixture()

  path <- write_validation_report(f$features, f$inventory, f$graph, out,
                                  f$app_path)
  expect_true(file.exists(path))

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- sub("## Conclusion\n", "## Conclusion\n\nSigned off by QA.\n", text,
              fixed = TRUE)
  writeLines(text, path)

  write_validation_report(f$features, f$inventory, f$graph, out, f$app_path)
  regenerated <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(regenerated, "Signed off by QA.", fixed = TRUE)
})

test_that("svt_render emits the report as a planned artifact", {
  out <- withr::local_tempdir()
  f <- report_fixture()

  svt_render(f$features, f$inventory, out_dir = out)

  expect_true(file.exists(file.path(out, "validation-report.md")))
  # Planned, so the lifecycle check does not treat it as an orphan.
  expect_true("validation-report.md" %in%
                plan_artifact_paths(f$features$records, f$inventory$features))
})

test_that("a manifest report: block survives normalization", {
  manifest <- list(
    features = list(),
    modules = list(),
    report = list(document_id = "VAL-1")
  )

  expect_equal(normalize_manifest(manifest)$report$document_id, "VAL-1")
})

test_that("Analysis findings reports graph-level codes, not just inventory ones", {
  path <- fixture_path("traditional_with_source")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  inv <- svt_inventory(feats$graph, feats)
  rendered <- render_validation_report(feats, inv, feats$graph, path)

  # The section claims completeness ("Every code raised during analysis"),
  # so omitting the Warnings table would make the signed document wrong.
  for (code in unique(feats$graph$warnings$code)) {
    expect_match(rendered$text, code, fixed = TRUE)
  }
})

test_that("the report and the index agree on the warning totals", {
  path <- fixture_path("traditional_with_source")
  feats <- svt_slice(svt_build_graph(svt_parse(path)))
  inv <- svt_inventory(feats$graph, feats)

  agg <- aggregate_warning_counts(feats, inv, feats$graph)
  report <- render_validation_report(feats, inv, feats$graph, path)$text

  # An auditor reading both artifacts must not be shown two different
  # numbers for the same run.
  for (i in seq_len(nrow(agg))) {
    expect_match(report,
                 paste0("| ", agg$code[i], " | ", agg$count[i], " |"),
                 fixed = TRUE)
  }
})
