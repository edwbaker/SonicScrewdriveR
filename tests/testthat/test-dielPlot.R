test_that("dielLabels rejects unknown format", {
  expect_error(dielLabels("neon tetra"), "Unknown format for dielLabels: neon tetra")
})

test_that("dielLabels gives expected output", {
  l <- dielLabels("clock24")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)

  l <- dielLabels("clock12")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)
})

test_that("dielPositions rejects unknown format", {
  expect_error(dielPositions("aardvark"), "Unknown format for dielPositions: aardvark")
})

test_that("dielPositions gives expected output", {
  l <- dielPositions("3hourly")
  expect_true(is.numeric(l))
  expect_equal(length(l), 8)

  l <- dielPositions("hours")
  expect_true(is.numeric(l))
  expect_equal(length(l), 24)

  l <- dielPositions("minutes")
  expect_true(is.numeric(l))
  expect_equal(length(l), 1440)
})

test_that("dielFraction rejects unknown formats", {
  expect_error(dielFraction(2, "wombat"), "Unknown input for dielFraction: wombat")
  expect_error(dielFraction(2, "hours", "worms"), "Unknown output for dielFraction: worms")
})

test_that("dielFraction gives expected output", {
  expect_equal(dielFraction(0, "minutes", "fraction"), 0)
  expect_equal(dielFraction(0, "minutes", "radians"), 0)
  expect_equal(dielFraction(12, "hours", "fraction"), 0.5)
  expect_equal(dielFraction(12, "hours", "radians"), pi)
  expect_equal(dielFraction(as.POSIXct("1985-02-11 12:00"), "POSIX", "fraction"), 0.5)
  expect_equal(dielFraction(24, "hours", "fraction"), 1)
  expect_equal(dielFraction(24, "hours", "radians"), 2*pi)
})

test_that("tzRot gives expected output", {
  expect_equal(tzRot(12), 0)
  expect_equal(tzRot(0, init=0), 0)
})

test_that("emptyDiel rejects unknown formats", {
  expect_error(emptyDiel(method="horse"), "Unknown method for emptyDiel: horse")
})

test_that("emptyDiel gives no warnings", {
  expect_silent(emptyDiel())
  expect_silent(emptyDiel(rot=0))
})

test_that("dielPlot gives no warnings", {
  expect_silent(dielPlot("2024-12-21", lat=54, lon=66))
  expect_silent(dielPlot("2024-06-21", lat=54, lon=66))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, rot="Solar Noon"))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, plot=c("Solar Noon", "Nadir")))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, legend=TRUE))
})

test_that("dielRings gives no warnings", {
  expect_silent(dielPlot(Sys.time(), lat=54, lon=0))
  expect_silent(dielRings("tom", "0000", "1200"))
})
