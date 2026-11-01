test_that("pulseIntervals finds the gaps between runs of pulses", {
  # Regular pulses every 10, each 5 samples long, with one gap of 65.
  onsets <- c(0, 10, 20, 30, 100, 110)
  p <- list(onsets=onsets, offsets=onsets + 5)
  r <- pulseIntervals(p, nsd=1)
  #The pulse at 30 runs to 34, so the silence begins at 35.
  expect_equal(r$onsets, 35)
  expect_equal(r$offsets, 100)
  # Previously the results were written at the index of the interval examined, so
  # the vectors were padded with zeroes and were longer than the gaps found.
  expect_equal(length(r$onsets), length(r$offsets))
})

test_that("pulseIntervals uses the nsd it is given", {
  # nsd was overwritten with 2 immediately before use.
  onsets <- c(0, 10, 20, 30, 100, 110)
  p <- list(onsets=onsets, offsets=onsets + 5)
  expect_equal(length(pulseIntervals(p, nsd=1)$onsets), 1)
  expect_warning(r <- pulseIntervals(p, nsd=10), "nsd=10")
  expect_equal(length(r$onsets), 0)
})

test_that("pulseIntervals copes with too few pulses", {
  # 2:length(odds) counted backwards for fewer than three onsets. Two pulses have
  # one gap between them, which cannot be an outlier of a set of one; one pulse
  # and no pulses have no gap at all, so there is nothing to say about them.
  expect_warning(r <- pulseIntervals(list(onsets=c(0, 10), offsets=c(5, 15))), "1 gap")
  expect_equal(length(r$onsets), 0)
  expect_silent(r <- pulseIntervals(list(onsets=0, offsets=5)))
  expect_equal(length(r$onsets), 0)
  expect_silent(r <- pulseIntervals(list(onsets=numeric(0), offsets=numeric(0))))
  expect_equal(length(r$onsets), 0)
})

test_that("pulseIntervals reports the silence between two pulses, not the pulse before it", {
  # Each interval started at the preceding pulse's onset rather than its offset,
  # so the whole of that pulse fell inside the interval reported as an area of no
  # pulses: an error of one pulse duration. pulses$offsets was handed in and
  # never read.
  onsets <- c(seq(100, 1000, by=100), 5000, 5100, 5200)
  p <- list(onsets=onsets, offsets=onsets + 50)

  r <- pulseIntervals(p)
  #The pulse at 1000 runs to 1049, so the silence begins at 1050, not at 1000.
  expect_equal(r$onsets, 1050)
  expect_equal(r$offsets, 5000)
  #The interval begins where a pulse ends and ends where a pulse begins.
  expect_true(r$onsets %in% p$offsets)
  expect_false(r$onsets %in% p$onsets)
  expect_true(r$offsets %in% p$onsets)
})

test_that("pulseIntervals measures no gap after a pulse the recording cut short", {
  # Such a pulse has an onset and no offset, so there is no end to measure the
  # gap after it from.
  onsets <- c(100, 200, 300, 400, 1500, 1600, 1700, 2000)
  p <- list(onsets=onsets, offsets=onsets[1:7] + 50)

  r <- pulseIntervals(p)
  expect_equal(r$onsets, 450)
  expect_equal(r$offsets, 1500)
})

test_that("pulseIntervals does not report an unusually short gap", {
  # The outlier test had a second arm selecting gaps that are abnormally short,
  # which is the opposite of an area of no pulses: this pulse train returned the
  # interval 16000 to 16010, which lay inside a pulse.
  onsets <- c(100, 2000, 4000, 6000, 8000, 10000, 12000,
              14000, 16000, 16010, 18000, 20000, 22000)
  p <- list(onsets=onsets, offsets=onsets + 5)

  r <- pulseIntervals(p)
  expect_length(r$onsets, 0)
  #The gap in question is 5 samples where every other gap is close to 2000, so a
  #two-sided test selects it and a one-sided test does not.
  gaps <- p$onsets[-1] - p$offsets[-length(p$offsets)]
  expect_true(any(gaps < mean(gaps) - 2*stats::sd(gaps)))
  expect_false(any(gaps > mean(gaps) + 2*stats::sd(gaps)))
})

test_that("pulseIntervals says so when no gap can reach the nsd it is given", {
  # An interval is kept only when a gap is more than nsd standard deviations
  # above the mean, but the gap is one of the values the mean and the standard
  # deviation are taken from, so the most one gap of k can reach is
  # (k-1)/sqrt(k). With the default nsd of 2 that is unreachable until there are
  # 6 gaps, so six or fewer pulses returned an empty result however extreme the
  # gap, and said nothing about why.
  onsets <- c(100, 200, 300, 1000, 1100, 1200)
  p <- list(onsets=onsets, offsets=onsets + 50)

  #Five gaps of 50, 50, 650, 50, 50: the long gap is thirteen times the others.
  expect_warning(r <- pulseIntervals(p), "1\\.789")
  expect_length(r$onsets, 0)
  #It is reported as soon as the cut-off is one it can reach.
  expect_silent(r <- pulseIntervals(p, nsd=1.5))
  expect_equal(r$onsets, 350)
  expect_equal(r$offsets, 1000)

  #Six gaps can reach 2.041, so seven pulses are enough for the default.
  seven <- c(100, 200, 300, 400, 1500, 1600, 1700)
  expect_silent(r <- pulseIntervals(list(onsets=seven, offsets=seven + 50)))
  expect_equal(r$onsets, 450)
  expect_equal(r$offsets, 1500)
})

test_that("pulseIntervals needs the offsets to measure a gap from", {
  # The gaps were taken from the onsets alone, so a result holding no offsets at
  # all was accepted and the intervals silently measured from the wrong end of
  # each pulse.
  expect_error(
    pulseIntervals(list(onsets=c(0, 10, 20, 30, 100, 110))),
    "must hold offsets"
  )
})

test_that("a detector's pulses run through pulseIntervals and pulseStats", {
  # The whole pipeline on one wave: nine pulses of 400 samples, eight of them
  # 1000 samples apart and the last far away, which is enough gaps for the
  # default nsd to be reachable.
  onsets <- c(seq(1001, 8001, by=1000), 15001)
  x <- numeric(20000)
  for (o in onsets) {
    x[o:(o + 399)] <- 1
  }
  w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)

  r <- pd_threshold(w, threshold=0.5)
  expect_equal(r$onsets, as.integer(onsets))
  expect_equal(r$offsets, as.integer(onsets + 400))

  stats <- pulseStats(r)
  expect_equal(stats$durations, rep(400, 9))
  expect_equal(stats$distances, c(rep(600, 7), 6600))

  gaps <- pulseIntervals(r)
  #The one long gap, from the end of the eighth pulse to the start of the ninth.
  expect_equal(gaps$onsets, 8401)
  expect_equal(gaps$offsets, 15001)
  #It holds every silent sample between the two pulses and no pulse sample at
  #all, which is what an area of no pulses is for. Reading either end one sample
  #out would break one of these three.
  expect_true(all(w@left[gaps$onsets:(gaps$offsets - 1)] == 0))
  expect_equal(w@left[gaps$onsets - 1], 1)
  expect_equal(w@left[gaps$offsets], 1)
})
