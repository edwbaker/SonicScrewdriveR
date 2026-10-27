#A quiet wave with two loud bursts in it. The windows pd_simple() averages over
#are 120 and 480 samples, so the bursts are made long enough to be seen by both.
pdSimpleWave <- function() {
  n <- 8000
  samples <- rep(0, n)
  samples[2001:3000] <- 10000
  samples[6001:7000] <- 10000
  tuneR::Wave(samples, samp.rate=4000, bit=16)
}

test_that("every input and output of the detection is returned", {
  res <- pd_simple(pdSimpleWave())
  expect_named(
    res,
    c("theta", "F_a", "F_1", "e_u", "e_v", "pulse", "onsets", "offsets")
  )
})

test_that("theta sits a fraction gamma of the way up the short-window energy", {
  wave <- pdSimpleWave()
  res <- pd_simple(wave, gamma=0.05)
  expect_equal(res$theta, min(res$e_u) + 0.05 * (max(res$e_u) - min(res$e_u)))
  #A larger gamma lifts the threshold.
  expect_gt(pd_simple(wave, gamma=0.5)$theta, res$theta)
})

test_that("the threshold returned is the one the pulses were found with", {
  res <- pd_simple(pdSimpleWave())
  expect_equal(res$pulse, res$e_u > res$F_a)
  #F_1 is the lower reference threshold, so it never exceeds F_a for alpha > 1.
  expect_true(all(res$F_1 <= res$F_a))
})

test_that("the two bursts are found, each with an onset before its offset", {
  res <- pd_simple(pdSimpleWave())
  expect_length(res$onsets, 2)
  expect_length(res$offsets, 2)
  expect_true(all(res$onsets < res$offsets))
  expect_true(all(diff(sort(c(res$onsets, res$offsets))) > 0))
})

test_that("the onsets found fall where the bursts begin", {
  res <- pd_simple(pdSimpleWave())
  #The energy windows are centred, so an onset is within half a window of the
  #first loud sample.
  expect_true(all(abs(res$onsets - c(2001, 6001)) <= 240))
})

test_that("a silent wave has no pulses rather than a threshold of NaN", {
  silent <- tuneR::Wave(rep(0, 4000), samp.rate=4000, bit=16)
  res <- pd_simple(silent)
  expect_equal(res$theta, 0)
  expect_false(any(res$pulse))
  expect_length(res$onsets, 0)
  expect_length(res$offsets, 0)
})

test_that("the window lengths given are the ones used", {
  wave <- pdSimpleWave()
  res <- pd_simple(wave, U=60, V=240)
  expect_equal(res$e_u, ste(wave, U=60))
  expect_equal(res$e_v, ste(wave, U=240))
})
