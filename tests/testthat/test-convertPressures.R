test_that("convert kPa to Pa", {
  expect_equal(convert2Pascals(1, input.unit="kPa"), 1000)
})
test_that("convert Pa to Pa no change", {
  expect_equal(convert2Pascals(1, input.unit="Pa"), 1)
})
test_that("convert dyne/cm2 to Pa", {
  expect_equal(convert2Pascals(10, input.unit="dyne_cm2"), 1)
})
test_that("convert2Pascals error on incorrect input", {
  expect_error(convert2Pascals(1, input.unit="J"))
})

test_that("convert Pa to dyne/cm2", {
  expect_equal(convert2dyne_cm2(1, input.unit="Pa"), 10)
})
test_that("convert kPa to dyne cm/2", {
  expect_equal(convert2dyne_cm2(1, input.unit="kPa"), 10000)
})
test_that("convert dyne/cm2 to dyne/cm2 no change", {
  expect_equal(convert2dyne_cm2(1, input.unit="dyne_cm2"),1)
})
test_that("convert2dyne_cm2 error on incorrect input", {
  expect_error(convert2dyne_cm2(1, input.unit="J"))
})
test_that("Convert 2 kPa", {
  expect_equal(convert2kPascals(1, input.unit="kPa"), 1)
  expect_equal(convert2kPascals(1000, input.unit="Pa"), 1)
})
