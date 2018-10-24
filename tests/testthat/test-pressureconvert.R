context("pressureconvert")

test_that("convert kPa to Pa", {
  expect_equal(convert2Pascals(1, input="kPa"), 1000)
})
test_that("convert Pa to Pa no change", {
  expect_equal(convert2Pascals(1, input="Pa"), 1)
})