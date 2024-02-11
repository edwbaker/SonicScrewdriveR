test_that("inputs are correct", {
  expect_error(frequencyStats("string"), "Expecting a Wave object")
  expect_error(frequencyStats(1), "Expecting a Wave object")
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE))
})
