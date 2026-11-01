test_that("a UTC offset becomes the Etc/GMT zone at that offset", {
  # The Etc/GMT zones are named with the opposite sign to the offsets they look
  # like, so copying the sign across put UTC+1 two hours out and UTC-5 ten.
  expect_equal(cleanTZ("UTC+1"), "Etc/GMT-1")
  expect_equal(cleanTZ("UTC-5"), "Etc/GMT+5")
  expect_equal(cleanTZ("UTC"), "Etc/GMT")

  noon <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz="UTC"))
  expect_equal(as.numeric(as.POSIXct("2024-01-01 13:00:00", tz=cleanTZ("UTC+1"))), noon)
  expect_equal(as.numeric(as.POSIXct("2024-01-01 07:00:00", tz=cleanTZ("UTC-5"))), noon)
  expect_equal(as.numeric(as.POSIXct("2024-01-01 12:00:00", tz=cleanTZ("UTC"))), noon)
})

test_that("a timezone name is returned as it is", {
  expect_equal(cleanTZ("Europe/London"), "Europe/London")
})
