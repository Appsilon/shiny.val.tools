test_that("the step reporter numbers each line against the planned total", {
  step <- new_step_reporter(c("First", "Second", "Third"))

  expect_message(step(), "\\[1/3\\] First")
  expect_message(step(), "\\[2/3\\] Second")
  expect_message(step(), "\\[3/3\\] Third")
})

test_that("the step reporter accepts an explicit message", {
  step <- new_step_reporter(c("a", "b"))
  expect_message(step("Something else"), "\\[1/2\\] Something else")
})

test_that("step indices are padded so the counter column stays aligned", {
  step <- new_step_reporter(as.character(1:10))
  expect_message(step(), "\\[ 1/10\\]")
})

test_that("the denominator describes the run actually happening", {
  # NULL entries drop out of the label vector, which is how the optional
  # testing stage shortens the pipeline.
  labels <- c("Parsing", "Graph", if (FALSE) "Surfaces", "Rendering")
  step <- new_step_reporter(labels)
  expect_message(step(), "\\[1/3\\]")
})

test_that("svt_validate counts the testing stage only when it runs", {
  out <- withr::local_tempdir()
  expect_message(
    svt_validate(fixture_path("traditional_basic"), out_dir = out),
    "\\[5/6\\] Deriving test surfaces"
  )

  out2 <- withr::local_tempdir()
  msgs <- capture_messages(
    svt_validate(fixture_path("traditional_basic"), out_dir = out2,
                 tests = "off")
  )
  expect_true(any(grepl("\\[5/5\\] Rendering artifacts", msgs)))
  expect_false(any(grepl("Deriving test surfaces", msgs)))
})

test_that("the render step names scaffolds when it writes them", {
  out <- withr::local_tempdir()
  msgs <- capture_messages(
    svt_validate(fixture_path("traditional_basic"), out_dir = out,
                 scaffold = TRUE)
  )
  expect_true(any(grepl("Rendering artifacts and scaffolds", msgs)))
})

test_that("the bar format carries a counter, the item, and no duplicate ETA", {
  fmt <- svt_bar_format("Building graph", "{.field {step}}")

  expect_match(fmt, "{cli::pb_current}/{cli::pb_total}", fixed = TRUE)
  expect_match(fmt, "{.field {step}}", fixed = TRUE)
  expect_match(fmt, "{cli::pb_eta_str}", fixed = TRUE)
  # pb_eta_str prints its own "ETA:" prefix; a second one is noise.
  expect_false(grepl("ETA {cli::pb_eta_str}", fmt, fixed = TRUE))
})

test_that("progress bars clear rather than accumulating in the transcript", {
  out <- withr::local_tempdir()
  msgs <- capture_messages(
    svt_validate(fixture_path("traditional_basic"), out_dir = out)
  )
  # Only the numbered step lines and the final summary survive the run.
  expect_false(any(grepl("Parsing files", msgs)))
  expect_false(any(grepl("Building graph", msgs)))
})
