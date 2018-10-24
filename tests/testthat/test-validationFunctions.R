context("validationFunctions")

test_that("RH is within limits", {
  expect_error(validateRH(-0.1))
})
test_that("RH is within limits", {
  expect_error(validateRH(100.1))
})
test_that("RH is within limits", {
  expect_null(validateRH(0))
})
test_that("RH is within limits", {
  expect_null(validateRH(100))
})


test_that("Kelvin is alway non-negative", {
  expect_error(validateKelvin(-1))
})
test_that("Kelvin can be 0 (maybe)", {
  expect_null(validateKelvin(0))
})