test_that("inputs are correct", {
  expect_error(frequencyStats("string"), "Expecting a Wave object")
  expect_error(frequencyStats(1), "Expecting a Wave object")
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE))
})

test_that("frequencyStats analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)

  expect_false(isTRUE(all.equal(frequencyStats(L), frequencyStats(R))))

  # frequencyStats returns a list, so allChannels() does not wrap it again.
  expect_equal(frequencyStats(tuneR::stereo(L, R)), list(frequencyStats(L), frequencyStats(R)))
  expect_equal(
    frequencyStats(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)),
    list(frequencyStats(L), frequencyStats(R))
  )
})

test_that("frequencyStats passes arguments to each channel", {
  sr <- 8000
  n <- 8000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, L)

  # warn=FALSE throughout: a pure tone leaves a single bin above the threshold
  # once lowcut is applied, which frequencyStats() rightly warns about.
  expect_equal(
    frequencyStats(st, lowcut=2, warn=FALSE),
    list(frequencyStats(L, lowcut=2, warn=FALSE), frequencyStats(L, lowcut=2, warn=FALSE))
  )
  expect_equal(frequencyStats(st, warn=FALSE), list(frequencyStats(L, warn=FALSE), frequencyStats(L, warn=FALSE)))
})

# A spectrum whose power is at or above half its maximum in exactly bins 5 to 7,
# which are 4, 5 and 6 kHz. frequencyStats() squares the amplitudes it is given.
knownSpectrum <- function(power) {
  return(cbind(seq(0, 10, length.out=11), sqrt(power)))
}
knownWave <- tuneR::sine(440, duration=4410, samp.rate=44100)

test_that("frequencyStats reads the edges of the threshold region", {
  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE)$`-3dB`
  # The lower edge used to be read one bin below the region, making every
  # bandwidth one bin too wide and every centre half a bin too low.
  expect_equal(r$min, 4)
  expect_equal(r$max, 6)
  expect_equal(r$bandwidth, 2)
  expect_equal(r$centre, 5)
  expect_equal(r$peak, 5)
})

test_that("frequencyStats returns one value per statistic when maxima tie", {
  # which(y == max(y)) returned every tied bin.
  spec <- knownSpectrum(c(0,0,0,0,4,4,4,0,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE)
  for (threshold in c("-3dB", "-10dB")) {
    for (stat in c("peak", "min", "max", "centre", "bandwidth", "Q")) {
      expect_length(r[[threshold]][[stat]], 1)
    }
  }
})

test_that("frequencyStats says so when there is no signal", {
  # The outer statistics were taken over an empty set, giving an infinite
  # frequency and an error about the sample rate.
  spec <- knownSpectrum(rep(0, 11))
  expect_error(frequencyStats(knownWave, wave_spec=spec, warn=FALSE), "No signal above lowcut")
})

test_that("frequencyStats separates the two thresholds", {
  # Power at half maximum over bins 5-7, and at a tenth over bins 4-8.
  spec <- knownSpectrum(c(0,0,0,0.5,2,4,2,0.5,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE)
  expect_equal(r$`-3dB`$min, 4)
  expect_equal(r$`-3dB`$max, 6)
  expect_equal(r$`-10dB`$min, 3)
  expect_equal(r$`-10dB`$max, 7)
  expect_gt(r$`-10dB`$bandwidth, r$`-3dB`$bandwidth)
})
