test_that("error in soundSpeed for unknown medium", {
  expect_error(soundSpeed(medium="pig semen"))
})

test_that("default value is for air, and correct", {
  expect_true(is.numeric(soundSpeed()))
  expect_equal(soundSpeed(), 343)
})

test_that("all works as a medium", {
  expect_type(soundSpeed(medium="all"), "list")
  expect_equal(ncol(soundSpeed(medium="all")), 2)
})

test_that("specific medium works", {
  expect_equal(soundSpeed(medium="steel"), 5941)
})

test_that("frequency and wavelength calculation works", {
  expect_equal(soundSpeed(f=330, wl=2), 660)
})

test_that("bulk modulus and density calculation works", {
  expect_equal(soundSpeed(bulkModulus =2.02e5, density=2), sqrt(1.01e5))
})
