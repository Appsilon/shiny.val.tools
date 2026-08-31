box::use(
  shiny[testServer],
  testthat[expect_equal, test_that],
)
box::use(app/view/counter)

test_that("counter increments", {
  testServer(counter$server, args = list(id = "test"), {
    session$setInputs(bump = 1)
    expect_equal(bumped(), 2)
    expect_equal(output$count, "2")
  })
})
