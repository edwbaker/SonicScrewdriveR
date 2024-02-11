test_that("convert C to K", {
  expect_equal(convert2Kelvin(0, input="C"), 273.15)
})
test_that("convert F to K", {
  expect_equal(round(convert2Kelvin(0, input="F"), 3), 255.372)
})
test_that("convert K to K is no change", {
  expect_equal(convert2Kelvin(0, input="K"), 0)
})


test_that("convert K to C", {
  expect_equal(convert2Celsius(0, input="K"), -273.15)
})
test_that("convert F to C", {
  expect_equal(convert2Celsius(32, input="F"), 0)
})
test_that("convert C to C is no change", {
  expect_equal(convert2Celsius(0, input="C"), 0)
})


test_that("error on incorrect unit convert2Celsius", {
  expect_error(convert2Celsius(0, input="Q"))
})
test_that("error on incorrect unit convert2Kelvin", {
  expect_error(convert2Kelvin(0, input="Q"))
})


test_that("nobody has created a working convert to Farenheit function", {
  expect_error(convert2Fahrenheit(0), "Implementation of this function is against the politcial beliefs of the author.")
})
