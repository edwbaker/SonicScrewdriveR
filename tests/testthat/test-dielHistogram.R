#Two times in the first hour of the day and one in the middle of it, so that the
#counts of the bins differ and the scaling can be read off them.
histTimes <- function() {
  as.POSIXct(
    c("2024-06-15 00:30:00", "2024-06-15 00:45:00", "2024-06-15 12:30:00"),
    tz="UTC"
  )
}

test_that("a bin size the function does not know is refused by name", {
  expect_error(
    dielHistogram(histTimes(), by="fortnight"),
    "Unknown value of by for dielHistogram: fortnight"
  )
})

test_that("the day is divided into bins of the size asked for", {
  emptyDiel()
  expect_equal(nrow(dielHistogram(histTimes(), by="hour")), 24)
  expect_equal(nrow(dielHistogram(histTimes(), by="30minute")), 48)
  expect_equal(nrow(dielHistogram(histTimes(), by="15minute")), 96)
})

test_that("the bins returned cover the whole circle without gaps", {
  emptyDiel()
  data <- dielHistogram(histTimes(), by="hour")
  expect_equal(colnames(data), c("from", "to", "value"))
  expect_equal(data$from[1], 0)
  expect_equal(data$to[nrow(data)], 2*pi)
  expect_equal(data$from[-1], data$to[-nrow(data)])
})

test_that("the times are counted into the bin of the day they fall in", {
  emptyDiel()
  data <- dielHistogram(histTimes(), by="hour")
  #Two times in the first hour and one in the thirteenth, scaled so the fullest
  #bin reaches the top of the limits.
  expect_equal(which(data$value > 0), c(1, 13))
  expect_equal(data$value[1], 1)
  expect_equal(data$value[13], 0.5)
})

test_that("the counts are scaled to the limits of the plot", {
  emptyDiel()
  data <- dielHistogram(histTimes(), by="hour", limits=c(1, 3))
  #The fullest bin reaches the width of the limits, here two rather than one.
  expect_equal(data$value[1], 2)
  expect_equal(data$value[13], 1)
})

test_that("a maximum given scales the bins against it, not against the fullest", {
  emptyDiel()
  data <- dielHistogram(histTimes(), by="hour", maxval=4)
  expect_equal(data$value[1], 0.5)
  expect_equal(data$value[13], 0.25)
  expect_lt(max(data$value), diff(c(1, 2)))
})

test_that("presence.only reports which bins hold times, not how many", {
  emptyDiel()
  data <- dielHistogram(histTimes(), by="hour", presence.only=TRUE)
  expect_equal(sort(unique(data$value)), c(0, 1))
  expect_equal(which(data$value == 1), c(1, 13))
})

test_that("the timezone given decides which bin a time falls in", {
  emptyDiel()
  utc <- dielHistogram(histTimes(), by="hour", timezone="UTC")
  #Four hours behind UTC in June, so the two times just after midnight fall in
  #the evening of the day before.
  east <- dielHistogram(histTimes(), by="hour", timezone="America/New_York")
  expect_equal(which(utc$value > 0), c(1, 13))
  expect_equal(which(east$value > 0), c(9, 21))
})

test_that("the bins are read in the timezone of the times when none is given", {
  emptyDiel()
  times <- as.POSIXct("2024-06-15 00:30:00", tz="America/New_York")
  expect_equal(
    dielHistogram(times, by="hour"),
    dielHistogram(times, by="hour", timezone="America/New_York")
  )
})

test_that("a bin holding nothing is not drawn", {
  emptyDiel()
  #An empty bin has no polygon to draw, so the loop skips it rather than drawing
  #one of no width.
  data <- dielHistogram(as.POSIXct("2024-06-15 00:30:00", tz="UTC"), by="hour")
  expect_equal(sum(data$value > 0), 1)
  expect_equal(data$value[1], 1)
})

test_that("the histogram is drawn on the plot that is already open", {
  emptyDiel()
  expect_silent(dielHistogram(histTimes(), by="hour"))
  expect_silent(dielHistogram(histTimes(), by="hour", col="red", rot=tzRot(2)))
})

test_that("a time on the hour is counted in the hour it begins", {
  emptyDiel()
  # The bins were right-closed, so each of these was counted in the hour before
  # it - except midnight, which include.lowest caught.
  mk <- function(t) as.POSIXct(paste("2024-06-15", t), tz="UTC")
  data <- dielHistogram(
    c(mk("00:00:00"), mk("01:00:00"), mk("02:00:00"), mk("23:00:00")),
    by="hour", timezone="UTC"
  )
  expect_equal(which(data$value > 0), c(1, 2, 3, 24))
})

test_that("a bin holds the times from its own start up to the next bin's", {
  emptyDiel()
  mk <- function(t) as.POSIXct(paste("2024-06-15", t), tz="UTC")
  data <- dielHistogram(
    c(mk("01:00:00"), mk("01:30:00"), mk("01:59:59"), mk("02:00:00")),
    by="hour", timezone="UTC", maxval=3
  )
  # Three times in the 01:00 bin and the one on the next boundary in the 02:00
  # bin.
  expect_equal(data$value[2], 1)
  expect_equal(data$value[3], 1/3)
  expect_equal(sum(data$value > 0), 2)
})

test_that("a time in the last bin of the day is counted", {
  emptyDiel()
  data <- dielHistogram(as.POSIXct("2024-06-15 23:59:59", tz="UTC"), by="hour",
                        timezone="UTC")
  expect_equal(which(data$value > 0), 24)
})

test_that("the smallest bins are counted the same way at both ends of the day", {
  emptyDiel()
  mk <- function(t) as.POSIXct(paste("2024-06-15", t), tz="UTC")
  data <- dielHistogram(c(mk("00:00:00"), mk("00:15:00"), mk("23:45:00")),
                        by="15minute", timezone="UTC")
  expect_equal(which(data$value > 0), c(1, 2, 96))
})

test_that("no times at all gives empty bins rather than an error", {
  emptyDiel()
  # The fullest bin held nothing, so every value was divided by zero and the
  # test for an empty bin failed with "missing value where TRUE/FALSE needed".
  # Filtering a dataset down to no detections in a window reaches that.
  none <- as.POSIXct(character(0), tz="UTC")
  data <- dielHistogram(none, by="hour")
  expect_equal(nrow(data), 24)
  expect_equal(data$value, rep(0, 24))
  expect_equal(dielHistogram(none, by="hour", presence.only=TRUE)$value, rep(0, 24))
})
