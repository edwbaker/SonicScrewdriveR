test_that("default to air with good known value", {
  suppressWarnings(expect_equal(wavelength(1), 343))
})

test_that("units default to m", {
  expect_equal(wavelength(1000, speed=300), wavelength(1000, speed=300, output.unit="m"))
})

test_that("cm calcualtion is correct", {
  expect_equal(wavelength(1000, speed=300, output.unit="m")*100, wavelength(1000, speed=300, output.unit="cm"))
})

test_that("error on invalid unit", {
  expect_error(wavelength(1, speed=300, output.unit="Fahrenheit is a moon unit"))
})

test_that("wavelength inverts frequencySound", {
  expect_equal(wavelength(frequencySound(wl=0.5)), 0.5)
})

test_that("wavelength refuses a frequency that has no wavelength", {
  # wavelength(0) was Inf and wavelength(-1000) was -0.343, a negative
  # wavelength, both returned without a word.
  expect_error(wavelength(0), "Frequency must be greater than zero to have a wavelength.")
  expect_error(wavelength(-1000), "Frequency must be positive.")
  expect_error(wavelength(-1000, output.unit="cm"), "Frequency must be positive.")
  expect_error(wavelength("1000"), "Frequency must be numeric.")
})

test_that("wavelength refuses a speed that is not a speed", {
  expect_error(wavelength(1000, speed="fast"), "Speed must be numeric")
})

test_that("wavelength still gives the wavelengths it gave", {
  expect_equal(wavelength(1000, speed=343), 0.343)
  expect_equal(wavelength(1000, speed=343, output.unit="cm"), 34.3)
  expect_equal(wavelength(c(500, 1000), speed=343), c(0.686, 0.343))
})
