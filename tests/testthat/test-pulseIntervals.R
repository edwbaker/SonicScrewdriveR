test_that("pulseIntervals finds the gaps between runs of pulses", {
  # Regular pulses every 10, with one gap of 70.
  p <- list(onsets=c(0, 10, 20, 30, 100, 110))
  r <- pulseIntervals(p, nsd=1)
  expect_equal(r$onsets, 30)
  expect_equal(r$offsets, 100)
  # Previously the results were written at the index of the interval examined, so
  # the vectors were padded with zeroes and were longer than the gaps found.
  expect_equal(length(r$onsets), length(r$offsets))
})

test_that("pulseIntervals uses the nsd it is given", {
  # nsd was overwritten with 2 immediately before use.
  p <- list(onsets=c(0, 10, 20, 30, 100, 110))
  expect_equal(length(pulseIntervals(p, nsd=1)$onsets), 1)
  expect_equal(length(pulseIntervals(p, nsd=10)$onsets), 0)
})

test_that("pulseIntervals copes with too few pulses", {
  # 2:length(odds) counted backwards for fewer than three onsets.
  expect_equal(length(pulseIntervals(list(onsets=c(0, 10)))$onsets), 0)
  expect_equal(length(pulseIntervals(list(onsets=0))$onsets), 0)
  expect_equal(length(pulseIntervals(list(onsets=numeric(0)))$onsets), 0)
})
