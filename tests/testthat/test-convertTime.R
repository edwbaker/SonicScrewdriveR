Sys.setenv(TZ='UTC')

test_that("Reject invalid input", {
  expect_error(convert2seconds(1, "dog"), "Unknown input to convert2seconds: dog")
})

test_that("Test conversions", {
  expect_equal(convert2seconds(1, input="seconds"), 1)
  expect_equal(convert2seconds(1, input="minutes"), 60)
  expect_equal(convert2seconds(1, input="hours"), 60*60)
  expect_equal(convert2seconds(1, input="days"), 60*60*24)
  expect_equal(convert2seconds(1, input="years"), 60*60*24*365)
  expect_error(convert2seconds("croc", "HHMM"), "HHMM input must be numeric")
  expect_equal(convert2seconds("1200", input="HHMM"), 60*60*12)
  expect_equal(convert2seconds(1200, input="HHMM"), 60*60*12)
  expect_equal(convert2seconds(as.POSIXct("2024-02-11 12:00", tz="UTC"), "POSIX", origin="day"), 60*60*12)
  expect_equal(convert2seconds(as.POSIXct("2024-02-11 12:00", tz="UTC"), "POSIX", origin="unix"), 1707652800)
  expect_equal(convert2seconds("0000", input="HHMM"), 0)
  expect_equal(convert2seconds("0100", input="HHMM"), 60*60)
  expect_equal(convert2seconds(0100, input="HHMM"), 60*60)
  expect_equal(convert2seconds(0130, input="HHMM"), 90*60)
  expect_equal(convert2seconds(2400, input="HHMM"), 60*60*24)
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
  expect_equal(humanTime(1, unit="hours"), "1 hour")
  expect_equal(humanTime(90, unit="minutes"), "1.5 hours")
  expect_equal(humanTime(1, unit="days"), "1 day")
})

test_that("humanTime passes NA through", {
  # humanBytes() already returned NA for NA; humanTime() aborted inside
  # validateTimeInSeconds() instead.
  expect_equal(humanTime(NA_real_), NA_character_)
  expect_equal(humanTime(c(1, NA, 90)), c("1 second", NA, "1.5 minutes"))
})
