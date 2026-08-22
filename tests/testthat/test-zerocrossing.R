zc <- function(v) zerocross(tuneR::Wave(v, samp.rate=44100, bit=16))

test_that("zerocross finds a crossing between samples of opposite sign", {
  expect_equal(zc(c(1, -1)), 2)
  expect_equal(zc(c(-1, 1)), 2)
})

test_that("zerocross counts a run of zeros once", {
  # Every sample equal to zero used to be reported as a crossing of its own, so a
  # run of them gave one crossing per sample.
  expect_equal(zc(c(1, 0, -1)), 2)
  expect_equal(zc(c(1, 0, 0, 0, -1)), 2)
  expect_equal(length(zc(c(1, 0, 0, 0, 0, 0, -1))), 1)
})

test_that("zerocross ignores a zero that is not a crossing", {
  # A zero between two samples of the same sign is not a crossing, but was
  # reported as one.
  expect_equal(length(zc(c(1, 0, 1))), 0)
  expect_equal(length(zc(c(-1, 0, -1))), 0)
  expect_equal(length(zc(c(0, 0, 0))), 0)
  expect_equal(length(zc(1)), 0)
})

test_that("zerocross counts a wave that begins at zero", {
  # There is nothing before the first sample to change sign against, but a wave
  # that starts at zero and departs from it does cross there.
  expect_equal(zc(c(0, 1, -1)), c(1, 3))
  expect_equal(zc(c(0, -1, 1)), c(1, 3))
})

test_that("zerocross finds two crossings per cycle of a sine", {
  w <- tuneR::sine(441, duration=44100, samp.rate=44100)
  # 441 Hz for one second is 441 cycles, so about 882 crossings.
  expect_gt(length(zerocross(w)), 870)
  expect_lt(length(zerocross(w)), 890)
})
