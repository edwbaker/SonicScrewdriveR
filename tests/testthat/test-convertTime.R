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
})
