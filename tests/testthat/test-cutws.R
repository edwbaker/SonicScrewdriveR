test_that("inputs are correct", {
  expect_error(cutws("string", from=1, to=2), "Expecting a Wave object")
  expect_error(cutws(1, from=1, to=2), "Expecting a Wave object")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1, to ="2"), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = "1", to =2), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1, to =2.5), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1.5, to =2), "In cutws both from and to must be integers")
  expect_silent(cutws(tuneR::sine(1000, duration=10), from = 1, to =2))
})

test_that("to must be greater than from", {
  expect_error(cutws(tuneR::sine(1000, duration=10), from=20, to = 1), "In cutws to must be greater than from")
})

test_that("plotting is ok in cutws", {
  expect_silent(cutws(tuneR::sine(1000, duration=10), from = 1, to =2, plot=TRUE))
})

test_that("cutws returns a Wave object", {
  expect_true(inherits(cutws(tuneR::sine(1000, duration=10), from = 1, to =2), "Wave"))
})

test_that("Handles Inf correctly", {
  w <- tuneR::sine(1000, duration=44100, samp.rate = 44100)
  expect_equal(length(cutws(w, from = 44001, to = Inf)@left), 100)
})
