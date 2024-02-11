test_that("default to air with good known value", {
  suppressWarnings(expect_equal(wavelength(1), 343))
})

test_that("units default to m", {
  expect_equal(wavelength(1000, speed=300), wavelength(1000, speed=300, unit="m"))
})

test_that("cm calcualtion is correct", {
  expect_equal(wavelength(1000, speed=300, unit="m")*100, wavelength(1000, speed=300, unit="cm"))
})

test_that("error on invalid unit", {
  expect_error(wavelength(1, speed=300, unit="Fahrenheit is a moon unit"))
})
