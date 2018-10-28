context("sDuration")

test_that("error if insufficient parameters", {
  expect_error(sDuration())
})

test_that("samp.rate paramter works", {
  expect_equal(sDuration(samp.rate=10), 0.1)
  expect_equal(sDuration(n=10, samp.rate=10), 1)
})

test_that("samp.rate from wave functions", {
  expect_silent(sDuration(wave=tuneR::silence(duration=1,samp.rate=10,bit=1)))
  expect_equal(sDuration(wave=tuneR::silence(duration=1,samp.rate=10,bit=1)), 0.1)
})