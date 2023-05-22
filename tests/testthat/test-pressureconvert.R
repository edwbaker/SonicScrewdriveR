context("pressureconvert")

test_that("convert kPa to Pa", {
  expect_equal(convert2Pascals(1, input="kPa"), 1000)
})
test_that("convert Pa to Pa no change", {
  expect_equal(convert2Pascals(1, input="Pa"), 1)
})
test_that("convert dyne/cm2 to Pa", {
  expect_equal(convert2Pascals(10, input="dyne_cm2"), 1)
})
test_that("convert2Pascals error on incorrect input", {
  expect_error(convert2Pascals(1, input="J"))
})

test_that("convert Pa to dyne/cm2", {
  expect_equal(convert2dyne_cm2(1, input="Pa"), 10)
})
test_that("convert kPa to dyne cm/2", {
  expect_equal(convert2dyne_cm2(1, input="kPa"), 10000)
})
test_that("convert dyne/cm2 to dyne/cm2 no change", {
  expect_equal(convert2dyne_cm2(1, input="dyne_cm2"),1)
})
test_that("convert2dyne_cm2 error on incorrect input", {
  expect_error(convert2dyne_cm2(1, input="J"))
})
