context("frequencyStats")

test_that("inputs are correct", {
  data(sheep, package="seewave")
  expect_error(frequencyStats("string"), "frequencyStats expects a Wave object")
  expect_error(frequencyStats(1), "frequencyStats expects a Wave object")
  expect_silent(frequencyStats(sheep))
})

test_that("values for the sheep from seewave are consistent", {
  data(sheep, package="seewave")
  data(sheep_frequencyStats, package="sonicscrewdriver")
  #expect_true(identical(frequencyStats(sheep), sheepFrequencyStats))
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE))
})
