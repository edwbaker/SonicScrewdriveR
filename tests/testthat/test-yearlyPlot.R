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
  expect_error(yearlyFraction(24, input.unit="firefly"), "Unknown input: firefly")
})

test_that("yearlyFraction gives expected results", {
  expect_equal(yearlyFraction(as.POSIXlt("2000-01-01 00:00:00"), year=2000), 0)
  expect_lte(abs(2*pi - yearlyFraction(as.POSIXlt("2000-12-31 23:59:59"))), 0.1)

  expect_equal(yearlyFraction(as.POSIXlt("2018-01-01 00:00:00"), year=2018, output.unit="fraction"), 0)
  expect_lte(abs(1 - yearlyFraction(as.POSIXlt("2018-12-31 23:59:59"), output.unit="fraction")), 0.1)

  expect_equal(yearlyFraction(0, input.unit="day"), 0)
  expect_lte(abs(2*pi - yearlyFraction(365, input.unit="day")), 0.1)
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

for (tz in sessionTimezones) {
  test_that(paste0("yearlyFraction places an instant the same way in session timezone ", tz), {
    withSessionTimezone(tz, {
      # 2024-01-01 00:00:00 UTC, the first moment of the year, as an instant
      # carrying no timezone. Read in a zone behind UTC this falls in the year
      # before, and read in one ahead it is already well into New Year's Day.
      expect_equal(yearlyFraction(naiveTime(1704067200), year=2024, output.unit="fraction"), 0)
      # The last day of the year, where the same shift ran off the other end.
      expect_equal(yearlyFraction(naiveTime(1735603200), year=2024, output.unit="fraction"), 365/366)
      # A time carrying a timezone is read in it.
      expect_equal(
        yearlyFraction(as.POSIXct("2024-01-01 00:00:00", tz="Australia/Sydney"),
                       year=2024, output.unit="fraction"),
        0
      )
    })
  })
}

for (tz in sessionTimezones) {
  test_that(paste0("yearlyFraction reads a POSIXlt in the timezone asked for in session ", tz), {
    withSessionTimezone(tz, {
      # The last evening of 2023 in UTC is already New Year's Day in Sydney, so
      # the day of the year depends on which zone the time is read in. A POSIXlt
      # used to ignore the timezone argument entirely.
      lt <- as.POSIXlt("2023-12-31 20:00:00", tz="UTC")
      expect_equal(yearlyFraction(lt, year=2023, output.unit="fraction"), 364/365)
      expect_equal(yearlyFraction(lt, year=2023, output.unit="fraction", timezone="Australia/Sydney"), 0)
    })
  })
}

test_that("yearlyFraction measures a date against its own year", {
  # The divisor came from the year argument, whose default is 2022, so a leap
  # year's 366 days of yday were divided by 365 and the last of them landed on
  # 2*pi, exactly where 1 January is.
  expect_equal(yearlyFraction(as.POSIXct("2024-12-31", tz="UTC")), 2*pi*365/366)
  expect_equal(yearlyFraction(as.POSIXct("2023-12-31", tz="UTC")), 2*pi*364/365)
  expect_equal(
    yearlyFraction(as.POSIXct("2024-02-29", tz="UTC"), output.unit="fraction"),
    59/366
  )
})

test_that("1 January is at zero and no date reaches a full turn", {
  for (year in c(2023, 2024)) {
    first <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz="UTC")
    last <- as.POSIXct(paste0(year, "-12-31 23:59:59"), tz="UTC")
    expect_equal(yearlyFraction(first), 0)
    expect_lt(yearlyFraction(last), 2*pi)
  }
})

test_that("29 February and 1 March are not drawn in the same place", {
  expect_false(isTRUE(all.equal(
    yearlyFraction(as.POSIXct("2024-02-29", tz="UTC")),
    yearlyFraction(as.POSIXct("2022-03-01", tz="UTC"))
  )))
})

test_that("the year argument does not move a POSIX date", {
  # It alone decided the length of the year, so the default moved every date
  # after February of a leap year a day.
  d <- as.POSIXct("2024-12-31", tz="UTC")
  expect_equal(yearlyFraction(d, year=2022), yearlyFraction(d, year=2024))
})

test_that("yearlyFraction reads a vector of times spanning more than one year", {
  # No single year argument is right for such a vector.
  d <- as.POSIXct(c("2023-12-31", "2024-12-31"), tz="UTC")
  expect_equal(yearlyFraction(d, output.unit="fraction"), c(364/365, 365/366))
})

test_that("a day number is measured against the year it is given", {
  expect_equal(
    yearlyFraction(365, year=2023, input.unit="day", output.unit="fraction"), 1
  )
  expect_equal(
    yearlyFraction(365, year=2024, input.unit="day", output.unit="fraction"), 365/366
  )
  # One year per day number, now that the question is asked of a vector.
  expect_equal(
    yearlyFraction(c(365, 365), year=c(2023, 2024), input.unit="day",
                   output.unit="fraction"),
    c(1, 365/366)
  )
})

test_that("the length of a year is answered for a vector of years", {
  expect_equal(.isLeapYear(c(1900, 2000, 2023, 2024)), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(.yearDays(c(2023, 2024)), c(365, 366))
})

test_that("yearlyPositions is refused more than one year by name", {
  expect_error(yearlyPositions(c(2023, 2024)), "one year at a time")
})
