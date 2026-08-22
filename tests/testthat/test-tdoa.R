#' Build a multichannel wave in which a signal reaches each channel at a
#' different time.
#'
#' @param delays Delay of each channel in samples, relative to the first.
#' @param n Length of the wave in samples.
#' @return A WaveMC object.
#' @noRd
delayedWave <- function(delays, n=4800, samp.rate=48000) {
  set.seed(42)
  signal <- stats::rnorm(n + 2000)
  data <- vapply(delays, function(d) signal[(1000 - d) + seq_len(n)], numeric(n))
  return(tuneR::WaveMC(data * 1e4, samp.rate=samp.rate, bit=32, pcm=FALSE))
}

test_that("tdoa finds known delays", {
  delays <- c(0, -121, 70)
  w <- delayedWave(delays)

  for (method in c("phat", "cc", "envelope")) {
    d <- tdoa(w, method=method, interpolate=FALSE)
    expect_equal(d$channel, 1:3)
    expect_equal(d$samples, delays)
    expect_equal(d$delay, delays/48000)
    #The reference channel correlates perfectly with itself, but for the
    #frequency the correlation cannot use: the mean is removed before
    #correlating, so no phase is left at 0Hz for the phase transform to weight.
    expect_equal(d$r[1], 1, tolerance=1e-3)
    expect_true(all(d$r > 0.5))
  }
})

test_that("tdoa correlates envelopes when the waveforms do not match", {
  #The same two pulses reach both channels, but on a different carrier
  #frequency, as happens when the path to each microphone favours different
  #frequencies. The waveforms have nothing in common, the envelopes have
  #everything.
  n <- 4800
  t <- seq_len(n + 400)
  pulses <- exp(-((t-1200)^2)/(2*200^2)) + exp(-((t-3000)^2)/(2*200^2))
  w <- tuneR::WaveMC(
    cbind(
      (pulses * sin(2*pi*8000*t/48000))[200 + seq_len(n)],
      (pulses * sin(2*pi*11000*t/48000))[260 + seq_len(n)]
    ) * 1e4,
    samp.rate = 48000,
    bit = 32,
    pcm = FALSE
  )

  envelope <- tdoa(w, method="envelope", interpolate=FALSE)
  expect_lt(abs(envelope$samples[2] - -60), 10)
  expect_gt(envelope$r[2], 0.9)

  #The methods that correlate the waveforms find nothing like it.
  expect_gt(abs(tdoa(w, method="cc", interpolate=FALSE)$samples[2] - -60), 10)
  expect_gt(abs(tdoa(w, method="phat", interpolate=FALSE)$samples[2] - -60), 10)
})

test_that("tdoa measures delays against the channel asked for", {
  w <- delayedWave(c(0, -121, 70))
  d <- tdoa(w, ref=2, interpolate=FALSE)
  expect_equal(d$samples, c(121, 0, 191))
  expect_equal(d$r[2], 1, tolerance=1e-3)
})

test_that("tdoa interpolates delays to a fraction of a sample", {
  #A delay of a whole number of samples plus a quarter, made by shifting the
  #phase of each frequency of a signal rather than its samples.
  set.seed(1)
  n <- 8192
  signal <- as.vector(stats::filter(stats::rnorm(n), rep(1/4, 4), sides=2))
  signal[is.na(signal)] <- 0
  k <- c(0:(n/2), (-n/2+1):-1)
  shifted <- Re(stats::fft(stats::fft(signal) * exp(-2i*pi*k*3.75/n), inverse=TRUE))/n
  w <- tuneR::WaveMC(cbind(signal, shifted) * 1e4, samp.rate=48000, bit=32, pcm=FALSE)

  expect_equal(tdoa(w, interpolate=FALSE)$samples[2], 4)
  expect_equal(tdoa(w)$samples[2], 3.75, tolerance=0.15)
  expect_equal(tdoa(w, method="cc")$samples[2], 3.75, tolerance=0.15)
  expect_lt(abs(tdoa(w, method="envelope")$samples[2] - 3.75), 1)
})

test_that("tdoa correlates only the region and lags asked for", {
  w <- delayedWave(c(0, -121))

  expect_equal(tdoa(w, from=0.01, to=0.08, interpolate=FALSE)$samples[2], -121)
  expect_equal(tdoa(w, from=480, to=3840, units="samples", interpolate=FALSE)$samples[2], -121)

  #A delay of 121 samples is outside a limit of 100 samples, so the peak found
  #has to be somewhere else.
  limited <- tdoa(w, max.delay=100/48000, interpolate=FALSE)
  expect_true(abs(limited$samples[2]) <= 100)
})

test_that("tdoa names channels where the wave does", {
  w <- delayedWave(c(0, -121))
  expect_true(all(is.na(tdoa(w)$name)))

  colnames(w@.Data) <- c("FL", "FR")
  expect_equal(tdoa(w)$name, c("FL", "FR"))
})

test_that("tdoa works on stereo Wave objects", {
  w <- delayedWave(c(0, -121))
  stereo <- tuneR::Wave(
    left = w@.Data[,1],
    right = w@.Data[,2],
    samp.rate = 48000,
    bit = 32,
    pcm = FALSE
  )
  expect_equal(tdoa(stereo, interpolate=FALSE)$samples, c(0, -121))
})

test_that("tdoa handles silent channels", {
  w <- delayedWave(c(0, -121, 0))
  w@.Data[,3] <- 0
  d <- tdoa(w, interpolate=FALSE)
  expect_equal(d$samples[3], 0)
  expect_equal(d$r[3], 0)
})

test_that("tdoa rejects what it cannot correlate", {
  w <- delayedWave(c(0, -121))
  expect_error(tdoa(1), "Expecting a Wave or WaveMC object")
  expect_error(tdoa(tuneR::sine(440)), "two or more channels")
  expect_error(tdoa(w, ref=5), "ref must be a channel of the wave")
  expect_error(tdoa(w, ref=c(1,2)), "ref must be a channel of the wave")
  expect_error(tdoa(w, method="nonsense"), "Unknown method for tdoa")
  expect_error(tdoa(w, max.delay=-1), "max.delay must be a positive number")
  expect_error(tdoa(w, max.delay=0), "max.delay must be a positive number")
  expect_error(tdoa(w, max.delay=1e-9), "max.delay is shorter than one sample")
  expect_error(tdoa(w, from=1, to=1, units="samples"), "too short")
})

test_that(".parabolicPeak finds the vertex of a parabola", {
  #A parabola with its vertex at the middle point.
  expect_equal(.parabolicPeak(0, 1, 0), 0)
  #A curve with no peak between the neighbours is not interpolated.
  expect_equal(.parabolicPeak(0, 0, 0), 0)
  expect_equal(.parabolicPeak(1, 0, 1), 0)
  #The vertex is always between the neighbours of the peak.
  expect_lte(abs(.parabolicPeak(0.99, 1, 0)), 0.5)
  expect_equal(.parabolicPeak(0.5, 1, 0), -1/6)
  expect_equal(.parabolicPeak(0, 1, 0.5), 1/6)
})
