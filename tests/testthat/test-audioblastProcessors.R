test_that(".timesOfDay() rejects incorrect input", {
  expect_silent(.timesOfDay(date=as.Date(Sys.time()), lat=54, lon=0))
  expect_error(.timesOfDay(date=as.Date(Sys.time())))
  expect_error(.timesOfDay(date=as.Date(Sys.time()), lat=54))
  expect_error(.timesOfDay(date=as.Date(Sys.time()), lon=0))
  expect_error(.timesOfDay(lat=54, lon=0))
})

test_that(".timesOfDay() works as expected", {
  expect_equal(typeof(.timesOfDay()), "character")
  expect_equal(typeof(.timesOfDay(as.Date(Sys.time()), 54, 0)), "list")
  expect_equal(ncol(.timesOfDay(as.Date(Sys.time()), 54, 0)), 3)
})

test_that(".calcTimesOfDay() works as expected", {
  t <- c("day", "night", "dayandnight", "afternoon-evening")
  min <- max <- rep_len(NA, length(t))
  o <- .calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0)

  expect_equal(typeof(o), "list")
  expect_equal(ncol(o), 2)
  expect_equal(colnames(o), c("min", "max"))

  # Test specific values
  expect_equal(as.character(o[1,]), c("0427", "1946"))
  expect_equal(as.character(o[2,]), c("2248", "0124"))
  expect_equal(as.character(o[3,]), c("0000", "2359"))
  expect_equal(as.character(o[4,]), c("1206", "2248"))
})
