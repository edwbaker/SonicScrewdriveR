test_that("test yearlyLabels", {
  expect_equal(length(yearlyLabels()), 12)
  expect_equal(typeof(yearlyLabels()), "character")
})

test_that(".isLeapYear", {
  expect_false(.isLeapYear(1900))
  expect_true(.isLeapYear(2000))
  expect_true(.isLeapYear(2004))
  expect_false(.isLeapYear(2005))
})

test_that("yearlyPositions gives expected results", {
  expect_equal(length(yearlyPositions()), 12)
  expect_true(is.numeric(yearlyPositions()))

  # Test shift on leap year
  ny <- yearlyPositions(2001)
  ly <- yearlyPositions(2000)
  expect_equal(sum(ny < ly), 10)
  expect_true(all(c(ny,ly) <= 2*pi))
  expect_true(all(c(ny,ly) >= 0))
})

test_that("yearlyFraction gives expected results", {
  expect_equal(yearlyFraction(as.POSIXlt("2018-01-01 00:00:00")), 0)
  expect_lte(abs(2*pi - yearlyFraction(as.POSIXlt("2018-12-31 23:59:59"))), 0.1)

  expect_equal(yearlyFraction(as.POSIXlt("2018-01-01 00:00:00"), unit="fraction"), 0)
  expect_lte(abs(1 - yearlyFraction(as.POSIXlt("2018-12-31 23:59:59"), unit="fraction")), 0.1)
})

test_that("Plotting does not throw errors", {
  expect_silent(yearlyPlot(2018, lat=54, lon=2))
  expect_silent(yearlyPlot("2024", lat=54, lon=66))
  expect_silent(yearlyPlot("2024", lat=54, lon=0))
  expect_silent(yearlyPlot("2024", lat=54, lon=0, legend=TRUE))
})
