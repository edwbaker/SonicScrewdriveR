
test_that("Reject invalid input", {
  expect_error(convert2seconds(1, "dog"), "Unknown input.unit to convert2seconds: dog")
})

test_that("Test conversions", {
  expect_equal(convert2seconds(1, input.unit="seconds"), 1)
  expect_equal(convert2seconds(1, input.unit="minutes"), 60)
  expect_equal(convert2seconds(1, input.unit="hours"), 60*60)
  expect_equal(convert2seconds(1, input.unit="days"), 60*60*24)
  expect_equal(convert2seconds(1, input.unit="years"), 60*60*24*365)
  expect_error(convert2seconds("croc", "HHMM"), "HHMM input must be numeric")
  expect_equal(convert2seconds("1200", input.unit="HHMM"), 60*60*12)
  expect_equal(convert2seconds(1200, input.unit="HHMM"), 60*60*12)
  expect_equal(convert2seconds(as.POSIXct("2024-02-11 12:00", tz="UTC"), "POSIX", origin="day"), 60*60*12)
  # tolerance=0 because the tolerance expect_equal() allows is relative, and at
  # an epoch of this size it comes to about 25 seconds, so the comparison would
  # pass whether or not the seconds are right.
  expect_equal(convert2seconds(as.POSIXct("2024-02-11 12:00", tz="UTC"), "POSIX", origin="unix"), 1707652800, tolerance=0)
  expect_equal(convert2seconds("0000", input.unit="HHMM"), 0)
  expect_equal(convert2seconds("0100", input.unit="HHMM"), 60*60)
  expect_equal(convert2seconds(0100, input.unit="HHMM"), 60*60)
  expect_equal(convert2seconds(0130, input.unit="HHMM"), 90*60)
  expect_equal(convert2seconds(2400, input.unit="HHMM"), 60*60*24)
})

test_that("Human time", {
  expect_equal(humanTime(1), "1 second")
  expect_equal(humanTime(2), "2 seconds")
  expect_equal(humanTime(60), "1 minute")
  expect_equal(humanTime(60*2), "2 minutes")
  expect_equal(humanTime(60*60), "1 hour")
  expect_equal(humanTime(60*60*2), "2 hours")
  expect_equal(humanTime(60*60*24), "1 day")
  expect_equal(humanTime(60*60*24*2), "2 days")
  expect_equal(humanTime(0), "0 seconds")
  expect_equal(humanTime(0.5), "0.5 seconds")
})

test_that("humanTime is vectorised", {
  # Previously this raised "the condition has length > 1".
  expect_equal(
    humanTime(c(1, 30, 90, 60*60*2)),
    c("1 second", "30 seconds", "1.5 minutes", "2 hours")
  )
  expect_equal(length(humanTime(numeric(0))), 0)
})

test_that("humanTime rounds, and can be asked not to", {
  expect_equal(humanTime(100), "1.667 minutes")
  expect_equal(humanTime(100, digits=1), "1.7 minutes")
  expect_equal(humanTime(100, digits=NULL), paste(100/60, "minutes"))
})

test_that("humanTime converts from other units", {
  expect_equal(humanTime(1, input.unit="hours"), "1 hour")
  expect_equal(humanTime(90, input.unit="minutes"), "1.5 hours")
  expect_equal(humanTime(1, input.unit="days"), "1 day")
})

test_that("humanTime passes NA through", {
  # humanBytes() already returned NA for NA; humanTime() aborted inside
  # validateTimeInSeconds() instead.
  expect_equal(humanTime(NA_real_), NA_character_)
  expect_equal(humanTime(c(1, NA, 90)), c("1 second", NA, "1.5 minutes"))
})

for (tz in sessionTimezones) {
  test_that(paste0("convert2seconds reads a POSIX time the same way in session timezone ", tz), {
    withSessionTimezone(tz, {
      # 2024-12-16 03:20:00 UTC, as an instant carrying no timezone of its own.
      # This used to be read in the session's zone, so the seconds since the
      # start of the day moved with the machine.
      expect_equal(convert2seconds(naiveTime(1734319200), "POSIX", origin="day"), 12000)
      # A time that carries a timezone is read in that timezone.
      expect_equal(
        convert2seconds(as.POSIXct("2024-12-16 03:20:00", tz="Australia/Sydney"), "POSIX", origin="day"),
        12000
      )
      # Naming a timezone overrides the one the time carries.
      expect_equal(
        convert2seconds(as.POSIXct("2024-12-16 03:20:00", tz="UTC"), "POSIX", origin="day",
                        timezone="Australia/Sydney"),
        51600
      )
      # An epoch is the same number of seconds wherever it is read.
      expect_equal(convert2seconds(naiveTime(1734319200), "POSIX", origin="unix"), 1734319200,
                   tolerance=0)
    })
  })
}

for (tz in sessionTimezones) {
  test_that(paste0("convert2seconds reads a POSIXlt in the timezone asked for in session ", tz), {
    withSessionTimezone(tz, {
      # as.POSIXlt() returns a time that is already broken down unchanged,
      # whatever timezone it is asked for, so a POSIXlt used to ignore the
      # timezone argument and answer in the one it carried.
      lt <- as.POSIXlt("2024-12-16 03:20:00", tz="UTC")
      expect_equal(convert2seconds(lt, "POSIX", origin="day"), 12000)
      expect_equal(
        convert2seconds(lt, "POSIX", origin="day", timezone="Australia/Sydney"),
        51600
      )
      # dielFraction() reads its times through the same path.
      expect_equal(dielFraction(lt, output.unit="fraction"), 12000/86400)
      expect_equal(dielFraction(lt, output.unit="fraction", timezone="Australia/Sydney"), 51600/86400)
    })
  })
}
