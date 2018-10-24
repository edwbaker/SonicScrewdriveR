context("test-tempconvert")

test_that("convert C to K", {
  expect_equal(convert2Kelvin(0, input="C"), 273.15)
})
test_that("convert F to K", {
  expect_equal(round(convert2Kelvin(0, input="F"), 3), 255.372)
})
test_that("convert K to K is no change", {
  expect_equal(convert2Kelvin(0, input="K"), 0)
})

test_that("nobody has created a working convert to Farenheit function", {
  expect_error(convert2Fahrenheit(0))
})