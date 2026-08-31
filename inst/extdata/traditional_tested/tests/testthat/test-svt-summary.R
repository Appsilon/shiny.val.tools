# @covers feature: summary

test_that("feature summary - observables respond to stimuli", {
  skip("SVT scaffold - assertions not written (SVT-W303)")

  shiny::testServer(testthat::test_path("..", ".."), {
    session$setInputs(
      factor = NULL,
      x      = NULL
    )
  })
})
