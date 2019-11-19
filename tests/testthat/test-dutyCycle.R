context("dutyCycle")

test_that("Correct value is output", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="unit", normalise=FALSE), 0.5)
})

test_that("Corect value is output in percantage mode", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="percent", normalise=FALSE), 50)
})
