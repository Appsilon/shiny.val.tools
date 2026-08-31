test_path_for <- function(app) file.path(fixture_path(app), "tests")

test_that("one row per test_that() block, support files flagged", {
  tbl <- build_tests_table(test_path_for("traditional_tested"),
                           fixture_path("traditional_tested"))

  expect_true(all(c("file", "line", "desc", "harness", "is_support",
                    "filled", "md5", "annotations", "touched_inputs",
                    "touched_outputs", "called_functions",
                    "server_target") %in% names(tbl)))
  expect_true("tests/testthat/test-doubled.R" %in% tbl$file)
  # helper-setup.R and tests/testthat.R define no test_that() blocks, so
  # they contribute no rows at all.
  expect_false(any(grepl("helper-setup", tbl$file)))
  expect_equal(tbl$desc[tbl$file == "tests/testthat/test-misc.R"],
               "arithmetic still works")
})

test_that("harness is testserver only when the block calls testServer()", {
  tbl <- build_tests_table(test_path_for("traditional_tested"),
                           fixture_path("traditional_tested"))
  expect_equal(tbl$harness[tbl$file == "tests/testthat/test-doubled.R"],
               "testserver")
  expect_equal(tbl$harness[tbl$file == "tests/testthat/test-ratio.R"], "unit")
})

test_that("a block still carrying the scaffold marker is not filled", {
  tbl <- build_tests_table(test_path_for("traditional_tested"),
                           fixture_path("traditional_tested"))
  expect_false(tbl$filled[tbl$file == "tests/testthat/test-svt-summary.R"])
  expect_true(tbl$filled[tbl$file == "tests/testthat/test-doubled.R"])
})

test_that("setInputs and output reads are collected from the block", {
  tbl <- build_tests_table(test_path_for("traditional_tested"),
                           fixture_path("traditional_tested"))
  row <- which(tbl$file == "tests/testthat/test-doubled.R")
  expect_equal(tbl$touched_inputs[[row]], "x")
  expect_equal(tbl$touched_outputs[[row]], "doubled")
  expect_true("double_it" %in% tbl$called_functions[[row]] ||
                "doubled_value" %in% tbl$called_functions[[row]])
})

test_that("@covers annotations are parsed from the preceding comment run", {
  tbl <- build_tests_table(test_path_for("traditional_tested"),
                           fixture_path("traditional_tested"))
  expect_equal(tbl$annotations[[which(tbl$file ==
                                        "tests/testthat/test-doubled.R")]],
               "feature:doubled")
  expect_equal(tbl$annotations[[which(tbl$file ==
                                        "tests/testthat/test-misc.R")]],
               character())
})

test_that("parse_covers reads every declared kind and splits lists", {
  expect_equal(parse_covers("# @covers feature: a, b"),
               c("feature:a", "feature:b"))
  expect_equal(parse_covers("#' @covers module: app/view/x"),
               "module:app/view/x")
  expect_equal(parse_covers("# @covers fn: compute"), "fn:compute")
  expect_equal(parse_covers("# @covers nonsense: x"), character())
  expect_equal(parse_covers("# just a comment"), character())
})

test_that("a missing test tree yields the canonical empty table", {
  tbl <- build_tests_table(file.path(fixture_path("traditional_basic"),
                                     "tests"),
                           fixture_path("traditional_basic"))
  expect_equal(nrow(tbl), 0L)
  expect_true("harness" %in% names(tbl))
})

test_that("test files never enter the app's enumerated source", {
  files <- enumerate_app_files(fixture_path("traditional_tested"))
  expect_false(any(grepl("^tests/", files)))
})
