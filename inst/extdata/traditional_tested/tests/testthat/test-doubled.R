# @covers feature: doubled

test_that("doubled reflects the input", {
  shiny::testServer(testthat::test_path("..", ".."), {
    session$setInputs(x = 2)
    expect_equal(doubled_value(), 4)
    expect_equal(output$doubled, "Result: 4")
  })
})
