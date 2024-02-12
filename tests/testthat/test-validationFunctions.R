test_that("RH is numeric", {
  expect_error(validateRH("string"), "RH must be numeric")
})

test_that("RH is within limits", {
  expect_error(validateRH(-0.1), "Realtive humidity must be between 0 and 100.")
  expect_error(validateRH(100.1), "Realtive humidity must be between 0 and 100.")
  expect_equal(validateRH(0), 0)
  expect_equal(validateRH(100), 100)
})

test_that("Bulk modulus is numeric", {
  expect_error(validateBulkModulus("string"), "Bulk modulus must be numeric")
})

test_that("Bulk modulus is not negative", {
  expect_error(validateBulkModulus(-1), "Bulk modulus must not be negative.")
  expect_equal(validateBulkModulus(0), 0)
  expect_equal(validateBulkModulus(1), 1)
})

test_that("Kelvin is alway non-negative", {
  expect_error(validateKelvin(-1), "Temperatures must be above 0K.")
  expect_equal(validateKelvin(0),0)
})

test_that("Kelvin is numeric" , {
  expect_error(validateKelvin("string"), "Kelvin must be numeric")
})

test_that("Speed is numeric", {
  expect_error(validateSpeed("string"), "Speed must be numeric")
  expect_equal(validateSpeed(1), 1)
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

test_that("wavelength is numeric", {
  expect_error(validateWavelength("length"), "Wavelength must be numeric")
  expect_equal(validateWavelength(1), 1)
})

test_that("wavelength is not negative", {
  expect_error(validateWavelength(-1), "Wavelength must not be negative.")
  expect_equal(validateWavelength(0), 0)
  expect_equal(validateWavelength(1), 1)
})

test_that("density is numeric", {
  expect_error(validateDensity("length"), "Density must be numeric")
  expect_equal(validateDensity(1), 1)
})

test_that("density is not negative", {
  expect_error(validateDensity(-1), "Density must not be negative.")
  expect_equal(validateDensity(0), 0)
  expect_equal(validateDensity(1), 1)
})

test_that("timezone corrections work", {
  expect_equal(cleanTZ("UTCa"), "Etc/GMTa")
})

test_that("validateIsWave works", {
  expect_error(validateIsWave("string"), "Expecting a Wave object")
  expect_equal(validateIsWave(tuneR::sine(1)), tuneR::sine(1))
})

test_that("validateIsWaveMC works", {
  expect_error(validateIsWaveMC("string"), "Expecting a WaveMC object")
  expect_error(validateIsWaveMC(tuneR::sine(1)), "Expecting a WaveMC object")

  # Generate a WaveMC object
  x <- seq(0, 2*pi, length = 44100)
  channel <- round(32000 * sin(440 * x))
  mc <- tuneR::WaveMC(data = channel, samp.rate = 44100, bit = 16, pcm = TRUE)
  expect_true(inherits(validateIsWaveMC(mc), "WaveMC"))
})

test_that("Spectrum validation", {
  data(sheep, package="seewave")
  s <- seewave::spec(sheep, plot=FALSE)
  expect_equal(validateSpectrum(s), s)

  expect_error(validateSpectrum("string"), "Spectrum must be double.")
  expect_error(validateSpectrum(2.3), "Spectrum must be a matrix.")

  m <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE)
  expect_error(validateSpectrum(m), "Spectrum must have two columns.")

  m <- matrix(NA_real_, nrow = 0, ncol = 2)
  expect_error(validateSpectrum(m), "Spectrum must have one or more rows.")

  m <- matrix(c(1,2,3,NA), nrow = 2, ncol = 2)
  expect_error(validateSpectrum(m, coerceNA=FALSE), "No NA allowedin spectra.")
  expect_silent(validateSpectrum(m, coerceNA=TRUE))

  m <- matrix(c(1,2,3,-4), nrow = 2, ncol = 2)
  expect_error(validateSpectrum(m, coerceNegative =FALSE), "No negative values in spectrum.")
  expect_silent(validateSpectrum(m, coerceNegative =TRUE))
})

test_that("validate comparable spectra", {
  m <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
  n <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
  expect_silent(validateComparableSpectra(m,n))

  n <- matrix(c(1,3,3,4), nrow = 2, ncol = 2)
  expect_error(validateComparableSpectra(m,n), "Spectra must have same frequency bins.")

  n <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
  expect_error(validateComparableSpectra(m,n), "Spectra must have equal number of rows.")
})

test_that("validate time in seconds", {
  expect_error(validateTimeInSeconds("string"), "Time in Seconds must be numeric.")
  expect_error(validateTimeInSeconds(-1), "Time in Seconds cannot be negative")
  expect_silent(validateTimeInSeconds(-1, coerceNegative=TRUE))
  expect_equal(validateTimeInSeconds(0), 0)
  expect_equal(validateTimeInSeconds(1), 1)
  expect_error(validateTimeInSeconds(c(1,500), max_t = 400), "Time in Seconds cannot be longer than max_t")
  expect_equal(validateTimeInSeconds(c(1,500), max_t = 400, coerceMaximum=TRUE), c(1,400))
})
