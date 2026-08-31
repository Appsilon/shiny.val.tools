validated <- function(app, out_dir, ...) {
  suppressMessages(svt_validate(fixture_path(app), out_dir = out_dir, ...))
}

test_that("svt_validate writes both traceability artifacts by default", {
  d <- withr::local_tempdir()
  res <- validated("traditional_tested", d)

  expect_true(file.exists(file.path(d, "traceability.json")))
  expect_true(file.exists(file.path(d, "traceability.md")))
  expect_length(res$traceability, 2L)
})

test_that("no test tree means no traceability artifacts", {
  d <- withr::local_tempdir()
  validated("traditional_basic", d)

  expect_false(file.exists(file.path(d, "traceability.json")))
  expect_false(file.exists(file.path(d, "traceability.md")))
})

test_that("traceability.json is schema 1.0 and carries no absolute path", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  raw <- paste(readLines(file.path(d, "traceability.json")), collapse = "")
  parsed <- yaml::yaml.load(raw)

  expect_equal(parsed$schema_version, "1.0")
  expect_equal(parsed$inputs$test_path, "tests")
  expect_false(grepl(normalizePath(d, winslash = "/"), raw, fixed = TRUE))
  expect_true(all(c("entries", "orphan_tests", "summary") %in% names(parsed)))
})

test_that("the matrix, gaps and orphan sections report the run", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  md <- paste(readLines(file.path(d, "traceability.md")), collapse = "\n")

  expect_match(md, "exercised\\*\\*, never \\*\\*correct")
  expect_match(md, "\\| doubled \\|.*covered")
  expect_match(md, "\\| notes \\|.*uncovered")
  expect_match(md, "test-misc\\.R")
  expect_match(md, "SVT-W103")
  expect_match(md, "SVT-W301")
})

test_that("the Reviewers section survives regeneration", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  p <- file.path(d, "traceability.md")
  txt <- readLines(p)
  txt[grep("^- Developer", txt)] <- "- Developer: A. Reviewer Date: 2026-01-01"
  writeLines(txt, p)

  validated("traditional_tested", d)
  expect_match(paste(readLines(p), collapse = "\n"), "A. Reviewer")
})

test_that("doc stubs gain a Test coverage section", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  md <- paste(readLines(file.path(d, "doubled.md")), collapse = "\n")

  expect_match(md, "## Test coverage")
  expect_match(md, "Status: covered")
})

test_that("the index gains a Verification coverage section", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  md <- paste(readLines(file.path(d, "index.md")), collapse = "\n")

  expect_match(md, "## Verification coverage")
  expect_match(md, "\\[Traceability matrix\\]\\(traceability\\.md\\)")
  # Coverage codes join the app-wide aggregate rather than living only in
  # the matrix; the index and the report must not disagree on a count.
  expect_match(md, "SVT-W301")
})

test_that("the validation report states an assessed verification status", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d)
  md <- paste(readLines(file.path(d, "validation-report.md")), collapse = "\n")

  expect_match(md, "Test coverage was assessed")
  expect_no_match(md, "Test coverage was not assessed")
  expect_match(md, "No test-result report was supplied")
})

test_that("the report still disclaims coverage when the layer is off", {
  d <- withr::local_tempdir()
  validated("traditional_tested", d, tests = "surface")
  md <- paste(readLines(file.path(d, "validation-report.md")), collapse = "\n")

  expect_match(md, "Test coverage was not assessed")
})

test_that("two runs produce byte-identical traceability artifacts", {
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  validated("traditional_tested", d1)
  validated("traditional_tested", d2)

  for (f in c("traceability.json", "traceability.md")) {
    expect_equal(readLines(file.path(d1, f)), readLines(file.path(d2, f)),
                 info = f)
  }
})

test_that("a surface that moved while its test did not is flagged W307", {
  app <- withr::local_tempdir()
  file.copy(list.files(fixture_path("traditional_tested"), full.names = TRUE),
            app, recursive = TRUE)
  d <- withr::local_tempdir()

  suppressMessages(svt_validate(app, out_dir = d))
  expect_false(grepl("SVT-W307",
                     paste(readLines(file.path(d, "traceability.json")),
                           collapse = "")))

  # The app grows a stimulus; the covering test file is untouched.
  server <- file.path(app, "server.R")
  writeLines(sub("double_it\\(input\\$x\\)", "double_it(input$x) + input$offset",
                 readLines(server)), server)
  suppressMessages(svt_validate(app, out_dir = d))

  raw <- paste(readLines(file.path(d, "traceability.json")), collapse = "")
  expect_true(grepl("SVT-W307", raw, fixed = TRUE))
})
