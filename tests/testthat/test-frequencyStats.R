context("frequencyStats")

test_that("inputs are correct", {
  expect_error(frequencyStats("string"), "frequencyStats expects a Wave object")
  expect_error(frequencyStats(1), "frequencyStats expects a Wave object")
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE))
})
