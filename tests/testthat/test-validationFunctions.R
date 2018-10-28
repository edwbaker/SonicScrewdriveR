context("validationFunctions")

test_that("RJ is numeric", {
  expect_error(validateRH("string"), "RH must be numeric")
})

test_that("RH is within limits", {
  expect_error(validateRH(-0.1), "Realtive humidity must be between 0 and 100.")
  expect_error(validateRH(100.1), "Realtive humidity must be between 0 and 100.")
  expect_silent(validateRH(0))
  expect_silent(validateRH(100))
})

test_that("Kelvin is alway non-negative", {
  expect_error(validateKelvin(-1), "Temperatures must be above 0K.")
  expect_silent(validateKelvin(0))
})

test_that("Kelvin is numeric" , {
  expect_error(validateKelvin("string"), "Kelvin must be numeric")
})

test_that("Inputs to validateFreqIsPossible are valid", {
  expect_error(validateFreqIsPossible("string", samp.rate="string"), "Frequency must be numeric.")
  expect_error(validateFreqIsPossible(12), "Validation of frequency requires Wave object or samp.rate")
  expect_error(validateFreqIsPossible(12,wave=tuneR::silence(samp.rate=1000), f=1000), "Validation of frequency requires Wave object OR samp.rate")
  expect_error(validateFreqIsPossible(12, wave="string"), "Expecting a Wave object")
  expect_error(validateFreqIsPossible(12, samp.rate="string"), "samp.rate must be numeric")
})

test_that("validateFreqIsPossible rejects impossible frequencies", {
  expect_error(validateFreqIsPossible(501, wave=tuneR::sine(freq=10,samp.rate=1000)), "Frequency is greater than half sample rate.")
  expect_error(validateFreqIsPossible(501, samp.rate=1000), "Frequency is greater than half sample rate.")
})