test_that("Stop on unknown unit", {
  expect_error(referencePressure(output.unit="octopus"), "Unknown unit to referencePressure: octopus")
})

test_that("reference pressure", {
  expect_equal(referencePressure(output.unit="dyne_cm2"), 0.0002)
  expect_equal(referencePressure(output.unit="Pa"), 0.00002)
})

test_that("reference intensity", {
  expect_equal(referenceIntensity(output.unit="watt_cm2"), 10^-16)
})

test_that("Stop on unknown unit", {
  expect_error(referenceIntensity(output.unit="basking shark"), "Unknown unit to referenceIntensity: basking shark")
})
