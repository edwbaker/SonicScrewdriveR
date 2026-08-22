test_that("pulseStats measures durations and the gaps between pulses", {
  r <- pulseStats(list(onsets=c(10, 50, 90), offsets=c(20, 60, 100)))
  expect_equal(r$durations, c(10, 10, 10))
  expect_equal(r$distances, c(30, 30))
})

test_that("pulseStats copes with too few pulses", {
  # 2:l counted backwards for fewer than two pulses.
  r <- pulseStats(list(onsets=10, offsets=20))
  expect_equal(r$durations, 10)
  expect_equal(length(r$distances), 0)

  r <- pulseStats(list(onsets=numeric(0), offsets=numeric(0)))
  expect_equal(length(r$durations), 0)
  expect_equal(length(r$distances), 0)
})

test_that("pulseStats copes with a pulse the recording cut short", {
  # A recording can end part way through a pulse, leaving one more onset than
  # offset, which used to give a negative duration from the recycled pairing.
  r <- pulseStats(list(onsets=c(10, 50, 90), offsets=c(20, 60)))
  expect_equal(r$durations, c(10, 10))
  expect_true(all(r$durations > 0))
  expect_equal(r$distances, 30)
})
