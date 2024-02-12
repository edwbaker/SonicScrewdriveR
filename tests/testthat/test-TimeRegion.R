test_that("Reject incorrect unit", {
  expect_error(region("monkeys", 0, 10), "Unit must be one of samples, seconds, minutes, hours")
})

test_that("TimeRegion with samples", {
  tr <- region("samples", 0, 10)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 10)
  expect_equal(tr@unit, "samples")
  expect_equal(.timeRegion2samples(tr, 44100), c(0,10))
})

test_that("TimeRegion with seconds", {
  tr <- region("seconds", 0, 10)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 10)
  expect_equal(tr@unit, "seconds")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*10))
})

test_that("TimeRegion with minutes", {
  tr <- region("minutes", 0, 1)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 1)
  expect_equal(tr@unit, "minutes")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*60))
})

test_that("TimeRegion with hours", {
  tr <- region("hours", 0, 1)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 1)
  expect_equal(tr@unit, "hours")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*60*60))
})

test_that("Subsetting Wave by TimeRegion works", {
  w <- tuneR::sawtooth(440, duration=2*44100, samp.rate=44100, stereo=TRUE)
  w <- w[.seconds(1,2)]
  expect_equal(length(w@left), 44101)
  expect_equal(length(w@right), 44101)
})

test_that("0 and Inf are handled correctly", {
  w <- tuneR::sawtooth(440, duration=2*44100, samp.rate=44100, stereo=TRUE)
  w <- w[.seconds(0,Inf)]
  expect_equal(length(w@left), 88200)
})
