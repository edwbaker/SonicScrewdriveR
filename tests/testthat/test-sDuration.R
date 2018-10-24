context("sDuration")

test_that("error if insufficient parameters", {
  expect_error(sDuration())
})

test_that("samp.rate paramter works", {
  expect_equal(sDuration(samp.rate=10), 0.1)
  expect_equal(sDuration(n=10, samp.rate=10), 1)
})