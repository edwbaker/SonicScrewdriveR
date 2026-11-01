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

test_that("pulseStats refuses an offset that does not follow its onset", {
  # The pairing is positional and nothing checked the sign of the result, so
  # misaligned detector output became a negative duration. Truncating to the
  # shorter list handles one extra onset at the end, but an extra offset at the
  # front shifts every pair by one, which is what pd_dietrich2004() returned for
  # a recording that begins part way through a pulse.
  expect_error(
    pulseStats(list(onsets=c(500, 1500), offsets=c(100, 900, 1900))),
    "at or before the onset beside it"
  )
  #A pulse of no length is not a pulse either.
  expect_error(
    pulseStats(list(onsets=10, offsets=10)),
    "at or before the onset beside it"
  )
})

test_that("pulseStats refuses pulses that overlap", {
  # A pulse beginning before the one before it ends means the two lists describe
  # different pulses, and the distance between them would be negative.
  expect_error(
    pulseStats(list(onsets=c(10, 15), offsets=c(20, 30))),
    "beginning before the pulse before it ends"
  )
})

test_that("pulseStats accepts what every detector reports", {
  # The convention in R/pulseDetection.R exists so that this guard never fires on
  # a detector's own output.
  fixtures <- pulseFixtures()
  for (case in names(fixtures)) {
    for (detector in names(pulseDetectors())) {
      r <- pulseDetectors()[[detector]](fixtures[[case]]$wave)
      s <- pulseStats(r)
      info <- paste(detector, "-", case)
      expect_true(all(s$durations > 0), info=info)
      expect_true(all(s$distances > 0), info=info)
    }
  }
})
