context("validationFunctions")

test_that("RJ is numeric", {
  expect_error(validateRH("string"), "RH must be numeric")
})

test_that("RH is within limits", {
  expect_error(validateRH(-0.1), "Realtive humidity must be between 0 and 100.")
  expect_error(validateRH(100.1), "Realtive humidity must be between 0 and 100.")
  expect_equal(validateRH(0), 0)
  expect_equal(validateRH(100), 100)
})

test_that("Kelvin is alway non-negative", {
  expect_error(validateKelvin(-1), "Temperatures must be above 0K.")
  expect_equal(validateKelvin(0),0)
})

test_that("Kelvin is numeric" , {
  expect_error(validateKelvin("string"), "Kelvin must be numeric")
})

test_that("Inputs to validateFreqIsPossible are valid", {
  expect_error(validateFreqIsPossible("string", samp.rate="string"), "Frequency must be numeric.")
  expect_error(validateFreqIsPossible(12), "Frequency requires Wave object or samp.rate")
  expect_error(validateFreqIsPossible(12,wave=tuneR::silence(samp.rate=1000), samp.rate=1000), "Frequency requires Wave object OR samp.rate")
  expect_error(validateFreqIsPossible(12, wave="string"), "Expecting a Wave object")
  expect_error(validateFreqIsPossible(12, samp.rate="string"), "samp.rate must be numeric")
})

test_that("validateFreqIsPossible rejects impossible frequencies", {
  expect_error(validateFreqIsPossible(501, wave=tuneR::sine(freq=10, samp.rate=1000)), "Frequency is greater than half sample rate.")
  expect_error(validateFreqIsPossible(501, samp.rate=1000), "Frequency is greater than half sample rate.")
  expect_error(validateFreqIsPossible(-1, samp.rate=1000), "Frequency must be positive.")
})

test_that("validateFreqIsPossible passes allowable values", {
  expect_equal(validateFreqIsPossible(250, wave=tuneR::sine(freq=10,samp.rate=1000)), 250)
})


test_that("Inputs to validateBandwidthIsPossible are valid", {
  expect_error(validateBandwidthIsPossible("string", samp.rate="string"), "Bandwidth must be numeric.")
  expect_error(validateBandwidthIsPossible(12), "Bandwidth requires Wave object or samp.rate")
  expect_error(validateBandwidthIsPossible(12,wave=tuneR::silence(samp.rate=1000), samp.rate=10), "Bandwidth requires Wave object OR samp.rate")
  expect_error(validateBandwidthIsPossible(12, wave="string"), "Expecting a Wave object")
  expect_error(validateBandwidthIsPossible(12, samp.rate="string"), "samp.rate must be numeric")
})

test_that("validateBandwidthIsPossible rejects impossible frequencies", {
  expect_error(validateBandwidthIsPossible(501, wave=tuneR::sine(freq=10,samp.rate=1000)), "Bandwidth is greater than half sample rate.")
  expect_error(validateBandwidthIsPossible(501, samp.rate=1000), "Bandwidth is greater than half sample rate.")
  expect_error(validateBandwidthIsPossible(-1, samp.rate=1000), "Bandwidth must be positive.")
})

test_that("validateBandwidthIsPossible passes allowable values", {
  expect_equal(validateBandwidthIsPossible(250, wave=tuneR::sine(freq=10,samp.rate=1000)), 250)
})

test_that("validateQ inputs", {
  expect_error(validateQ("string"), "Q must be numeric.")
})

test_that("validateQ rejects negative input", {
  expect_error(validateQ(-1), "Q must be positive.")
  expect_equal(validateQ(1), 1)
})

test_that("validateDutyCycle inputs", {
  expect_error(validateDutyCycle("string"), "Duty cycle must be numeric.")
})

test_that("validateDutyCycle rejects out of range values", {
  expect_error(validateDutyCycle(2), "Duty cycle must be less than or equal to one.")
  expect_error(validateDutyCycle(-1), "Duty cycle must be greater than or equal to zero.")
  expect_equal(validateDutyCycle(0), 0)
  expect_equal(validateDutyCycle(1), 1)
})