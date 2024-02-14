test_that("ab_diel_traits with real data", {
  data <- audioblast(
    "data",
    "traits",
    "source" = "bio.acousti.ca",
    "id" = 33170,
    max_pages = 1)
  data2 <- ab_diel_traits(data, "2024-12-12", 54, 0)
  expect_equal(data2$value_min[1], 1538)
  expect_equal(data2$value_max[1], 1751)

  data2 <- data[, c("value", "value_min")]
  data2 <- ab_diel_traits(data2, "2024-12-12", 54, 0)
  expect_equal(data2$value_min[1], 1538)
  expect_equal(data2$value_max[1], 1751)

  data2 <- data[, c("value", "value_max")]
  data2 <- ab_diel_traits(data2, "2024-12-12", 54, 0)
  expect_equal(data2$value_min[1], 1538)
  expect_equal(data2$value_max[1], 1751)
})

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

test_that(".calcTimesofDay() gives correct warnings", {
  t <- "morning-evening"
  min <- max <- ""
  expect_silent(.calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0))
  t <- "morning-afternoon-evening"
  expect_warning(.calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0), "Cannot split on more than one '-'.")
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

  # Test that existing functions are not overwritten
  min[1] <- "1300"
  o <- .calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0)
  expect_equal(typeof(o), "list")
  expect_equal(ncol(o), 2)
  expect_equal(colnames(o), c("min", "max"))

  expect_equal(as.character(o[1,]), c("1300", "1946"))
  expect_equal(as.character(o[2,]), c("2248", "0124"))
  expect_equal(as.character(o[3,]), c("0000", "2359"))
  expect_equal(as.character(o[4,]), c("1206", "2248"))

  max[2] <- "0200"
  o <- .calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0)
  expect_equal(typeof(o), "list")
  expect_equal(ncol(o), 2)
  expect_equal(colnames(o), c("min", "max"))

  expect_equal(as.character(o[1,]), c("1300", "1946"))
  expect_equal(as.character(o[2,]), c("2248", "0200"))
  expect_equal(as.character(o[3,]), c("0000", "2359"))
  expect_equal(as.character(o[4,]), c("1206", "2248"))

  min[3] <- "0200"
  max[3] <- "0300"
  o <- .calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0)
  expect_equal(typeof(o), "list")
  expect_equal(ncol(o), 2)
  expect_equal(colnames(o), c("min", "max"))

  expect_equal(as.character(o[1,]), c("1300", "1946"))
  expect_equal(as.character(o[2,]), c("2248", "0200"))
  expect_equal(as.character(o[3,]), c("0200", "0300"))
  expect_equal(as.character(o[4,]), c("1206", "2248"))

  # Test overwrite
  o <- .calcTimesOfDay(t, min, max, as.Date("2024-08-08"), 54, 0, overwrite=TRUE)

  expect_equal(typeof(o), "list")
  expect_equal(ncol(o), 2)
  expect_equal(colnames(o), c("min", "max"))

  expect_equal(as.character(o[1,]), c("0427", "1946"))
  expect_equal(as.character(o[2,]), c("2248", "0124"))
  expect_equal(as.character(o[3,]), c("0000", "2359"))
  expect_equal(as.character(o[4,]), c("1206", "2248"))
})
