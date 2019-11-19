context("reference levels")

test_that("reference pressure", {
  expect_equal(referencePressure(unit="dyne_cm2"), 0.0002)
})
test_that("reference intensity", {
  expect_equal(referenceIntensity(unit="watt_cm2"), 10^-16)
})
