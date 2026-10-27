test_that("Conversion to degrees", {
  expect_equal(convert2degrees(pi/2), 90)
  expect_equal(convert2degrees(90, input.unit="degrees"), 90)
})

test_that("Conversion to radians", {
  expect_equal(convert2radians(90), pi/2)
  expect_equal(convert2radians(pi/2, input.unit="radians"), pi/2)
})

test_that("Reject unknown input", {
  expect_error(
    convert2degrees(90, input.unit="tuna"),
    "Unknown input.unit to convert2degrees(): tuna. Valid units are: degrees, radians",
    fixed=TRUE
  )
  expect_error(
    convert2radians(90, input.unit="garlic"),
    "Unknown input.unit to convert2radians(): garlic. Valid units are: degrees, radians",
    fixed=TRUE
  )
})
