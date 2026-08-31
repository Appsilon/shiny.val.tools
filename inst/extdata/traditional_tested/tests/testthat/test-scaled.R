test_that("scaled uses the default factor", {
  shiny::testServer(testthat::test_path("..", ".."), {
    session$setInputs(x = 2)
    expect_equal(output$scaled, "Result: 6")
  })
})
