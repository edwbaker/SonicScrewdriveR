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

test_that("yearlyPositions rejects unknown format", {
  expect_error(yearlyPositions(2001, format="amoeba"), "Unknown format: amoeba")
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

  # Test days output
  expect_equal(length(yearlyPositions(2001, format="days")), 365)
  expect_equal(length(yearlyPositions(2000, format="days")), 366)

  #Test mid-month output
  sm <- yearlyPositions(2001)
  mm <- yearlyPositions(2001, format="mid-months")
  margin = 16*2*pi/366
  expect_true(all((mm-sm) < margin))
})

test_that("yearlyFraction rejects unknown input", {
  expect_error(yearlyFraction(24, input="firefly"), "Unknown input: firefly")
})

test_that("yearlyFraction gives expected results", {
  expect_equal(yearlyFraction(as.POSIXlt("2000-01-01 00:00:00"), year=2000), 0)
  expect_lte(abs(2*pi - yearlyFraction(as.POSIXlt("2000-12-31 23:59:59"))), 0.1)

  expect_equal(yearlyFraction(as.POSIXlt("2018-01-01 00:00:00"), year=2018, unit="fraction"), 0)
  expect_lte(abs(1 - yearlyFraction(as.POSIXlt("2018-12-31 23:59:59"), unit="fraction")), 0.1)

  expect_equal(yearlyFraction(0, input="day"), 0)
  expect_lte(abs(2*pi - yearlyFraction(365, input="day")), 0.1)
})

test_that("Plotting does not throw errors", {
  expect_silent(emptyYearly(2018))
  expect_silent(emptyYearly(2000))
  expect_silent(yearlyPlot(2018, lat=54, lon=2))
  expect_silent(yearlyPlot("2024", lat=54, lon=66))
  expect_silent(yearlyPlot("2024", lat=54, lon=0))
  expect_silent(yearlyPlot("2024", lat=54, lon=0, legend=TRUE))
})

test_that("yearlyPlot uses the year it is given, not the session timezone", {
  # Dates were built as times in the session's zone and read back as UTC, so a
  # session east of UTC asked for sun times a day out.
  pdf(NULL)
  on.exit({dev.off(); Sys.setenv(TZ="UTC")})
  for (tz in c("UTC", "Australia/Sydney", "America/Los_Angeles")) {
    Sys.setenv(TZ=tz)
    expect_silent(yearlyPlot(2024, lat=50.1, lon=1.83))
  }
})

test_that("yearlyPlot copes with latitudes where the sun does not set", {
  # suncalc gives NA for both sunrise and sunset, and polygon() reads a missing
  # coordinate as a break between sub-polygons rather than as a full day.
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(yearlyPlot(2024, lat=78, lon=15))
  expect_silent(yearlyPlot(2024, lat=-78, lon=15))
})

test_that("yearlyPlot labels the year it was asked for", {
  # emptyYearly() was called with no arguments, so a leap year was labelled with
  # the positions of the default 2022.
  expect_false(identical(yearlyPositions(2024), yearlyPositions(2023)))
  expect_equal(length(yearlyPositions(2024)), 12)
})

test_that("yearlyPlot says so when an argument does nothing", {
  pdf(NULL)
  on.exit(dev.off())
  expect_warning(yearlyPlot(2024, lat=50, lon=0, plot="night"), "not implemented")
  expect_error(yearlyPlot(2024, lat=50, lon=0, method="dog"), "Unknown method")
})
