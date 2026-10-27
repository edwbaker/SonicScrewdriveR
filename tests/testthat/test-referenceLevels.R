test_that("Stop on unknown unit", {
  expect_error(
    referencePressure(output.unit="octopus"),
    "Unknown output.unit to referencePressure(): octopus. Valid units are: Pa, dyne_cm2",
    fixed=TRUE
  )
})

test_that("reference pressure", {
  expect_equal(referencePressure(output.unit="dyne_cm2"), 0.0002)
  expect_equal(referencePressure(output.unit="Pa"), 0.00002)
})

test_that("reference intensity", {
  expect_equal(referenceIntensity(output.unit="watt_cm2"), 10^-16)
})

test_that("Stop on unknown unit", {
  expect_error(
    referenceIntensity(output.unit="basking shark"),
    "Unknown output.unit to referenceIntensity(): basking shark. Valid units are: watt_cm2",
    fixed=TRUE
  )
})
