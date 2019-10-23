context("data2Wave")

test_that("only correct inputs are accepted", {
  expect_error(data2Wave(c("cat", "dog")))
  expect_silent(data2Wave(c(1,2,3)))
})

test_that("to must be greater than from", {
  expect_error(cutws(tuneR::sine(1000, duration=10), from=20, to = 1), "In cutws to must be greater than from")
})

test_that("plotting is ok in cutws", {
  expect_silent(cutws(tuneR::sine(1000, duration=10), from = 1, to =2, plot=TRUE))
})
