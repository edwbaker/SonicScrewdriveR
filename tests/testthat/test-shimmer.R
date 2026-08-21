# A tone of constant amplitude has no shimmer, and one whose amplitude alternates
# between two values has a shimmer of 20*log10 of their ratio.
tone <- function(freq=200, amps=1, duration=44100, samp.rate=44100) {
  t <- 0:(duration-1)
  amp <- rep_len(amps[((t %/% (samp.rate/freq)) %% length(amps)) + 1], duration)
  tuneR::Wave(amp * sin(2*pi*freq*t/samp.rate), samp.rate=samp.rate, bit=16)
}

test_that("a tone of constant amplitude has no shimmer", {
  for (freq in c(100, 200, 400)) {
    expect_equal(shimmer(tone(freq)), 0, tolerance=1e-3, info=paste("freq =", freq))
  }
})

test_that("shimmer measures the level difference between periods", {
  expect_equal(shimmer(tone(200, amps=c(1, 0.9))), 20*log10(1/0.9), tolerance=1e-3)
  expect_equal(shimmer(tone(200, amps=c(1, 0.5))), 20*log10(1/0.5), tolerance=1e-3)
})

test_that("shimmer does not depend on the frequency of the tone", {
  a <- shimmer(tone(100, amps=c(1, 0.8)))
  b <- shimmer(tone(400, amps=c(1, 0.8)))
  expect_equal(a, b, tolerance=1e-2)
})

test_that("shimmer is unchanged by inverting the waveform", {
  w <- tone(200, amps=c(1, 0.9))
  inverted <- tuneR::Wave(-w@left, samp.rate=w@samp.rate, bit=w@bit)

  expect_equal(shimmer(w), shimmer(inverted), tolerance=1e-3)
})

test_that("shimmer returns NA when there are too few periods", {
  w <- tuneR::Wave(rep.int(1, 10), samp.rate=44100, bit=16)

  expect_true(is.na(shimmer(w)))
})

test_that("shimmer rejects incorrect input", {
  expect_error(shimmer("not a wave"), "Expecting a Wave object")
})
