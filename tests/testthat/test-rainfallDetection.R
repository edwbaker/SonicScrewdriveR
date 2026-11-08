test_that("rainfallDetection analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  quiet <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  rain <- tuneR::Wave(round(pmax(pmin(rnorm(n) * 15000, 32767), -32768)), samp.rate=sr, bit=16)

  # Rain in one channel only. Before this was analysed channel by channel the
  # stereo file reported the quiet channel and the rain was missed.
  expect_equal(rainfallDetection(quiet), 0)
  expect_gt(rainfallDetection(rain), 0)

  expect_equal(
    rainfallDetection(tuneR::stereo(quiet, rain)),
    list(list(rainfallDetection(quiet)), list(rainfallDetection(rain)))
  )
  expect_equal(
    rainfallDetection(tuneR::WaveMC(cbind(quiet@left, rain@left), samp.rate=sr, bit=16)),
    list(list(rainfallDetection(quiet)), list(rainfallDetection(rain)))
  )
})

test_that("rainfallDetection passes arguments to each channel", {
  sr <- 8000
  n <- 8000
  set.seed(5)
  w <- tuneR::Wave(round(pmax(pmin(rnorm(n) * 15000, 32767), -32768)), samp.rate=sr, bit=16)
  st <- tuneR::stereo(w, w)

  expect_equal(rainfallDetection(st, Tmean=1e-3),
               list(list(rainfallDetection(w, Tmean=1e-3)), list(rainfallDetection(w, Tmean=1e-3))))
  expect_error(rainfallDetection(st, method="not a method"), "No valid method supplied.")
})

test_that("the rainfall PSD does not depend on the format the wave is held in", {
  sr <- 22050
  set.seed(101)
  # One recording, written five ways. The samples are quantised once and then
  # rescaled, so the waves hold the same audio and differ only in format.
  q <- round(rnorm(sr, sd=0.3) * 32767)
  w16 <- tuneR::Wave(q, samp.rate=sr, bit=16, pcm=TRUE)
  wfl <- tuneR::Wave(q / 32767, samp.rate=sr, bit=32, pcm=FALSE)
  w32 <- tuneR::Wave(round(q / 32767 * (2^31 - 1)), samp.rate=sr, bit=32, pcm=TRUE)
  w24 <- tuneR::Wave(round(q / 32767 * (2^23 - 1)), samp.rate=sr, bit=24, pcm=TRUE)
  # Eight bit PCM is unsigned about a midpoint, so it is the one depth where the
  # largest sample is not the largest excursion from silence. Centring it is what
  # keeps it with the others: dividing by 255 alone left it four times low.
  w8 <- tuneR::Wave(round(q / 32767 * 127) + 128, samp.rate=sr, bit=8, pcm=TRUE)

  # Before the PSD was put on a fixed scale these were 1.7e+03, 1.5e-06 and
  # 7.6e+12 for the same sound, and quieter recordings changed class with the
  # format: two of ten real rain recordings were rainfall at 16 bit and not
  # rainfall as 32 bit float.
  expect_equal(rainfallDetection(wfl), rainfallDetection(w16), tolerance=1e-6)
  expect_equal(rainfallDetection(w32), rainfallDetection(w16), tolerance=1e-6)
  expect_equal(rainfallDetection(w24), rainfallDetection(w16), tolerance=1e-6)
  # Eight bits of a loud recording is a coarse quantisation, so this is only
  # close, where the others are exact.
  expect_equal(rainfallDetection(w8), rainfallDetection(w16), tolerance=0.01)
  expect_gt(rainfallDetection(w16), 0)
})

test_that("the rainfall PSD is a power spectral density", {
  sr <- 22050
  set.seed(102)
  # Broadband noise spreads its power evenly, so the density of any band is the
  # variance of the samples over the Nyquist bandwidth. Nothing but a real
  # density satisfies that, and the published Tmean is a number in those units.
  x <- rnorm(4*sr, sd=0.25)
  w <- tuneR::Wave(round(x * 32767), samp.rate=sr, bit=16, pcm=TRUE)
  expect_equal(rainfallDetection(w), stats::var(x) / (sr/2), tolerance=0.1)
})

test_that("the rainfall mean threshold decides something", {
  sr <- 22050
  set.seed(103)
  # Quiet broadband noise: noise-like enough to clear Tsnr, far too quiet to be
  # rain. Over 200 draws of this the un-normalised mean cleared Tmean 200 times
  # out of 200, so the rule was Tsnr alone.
  w <- tuneR::Wave(round(rnorm(sr, sd=0.02) * 32767), samp.rate=sr, bit=16, pcm=TRUE)
  expect_equal(rainfallDetection(w), 0)
  # ...and it is the level that rejects it, not the signal-to-noise ratio.
  expect_gt(rainfallDetection(w, Tmean=1e-9), 0)
})

test_that("plain broadband noise is not rainfall", {
  sr <- 22050
  # A detector that fires on any broadband noise is not detecting rain. Over 200
  # draws of each of these the function used to report rainfall every time for
  # white noise and 170 times out of 200 for pink, at any level down to a few
  # least significant bits, because its level criterion could not fail.
  for (seed in 1:5) {
    set.seed(seed)
    white <- tuneR::Wave(round(rnorm(2*sr, sd=0.05) * 32767),
                         samp.rate=sr, bit=16, pcm=TRUE)
    expect_equal(rainfallDetection(white), 0)
  }
  for (seed in 6:10) {
    set.seed(seed)
    # Pink noise: a falling spectrum, so more of its power sits in the band than
    # white noise puts there, and it was the harder of the two to reject.
    n <- 2*sr
    nk <- n %/% 2 + 1
    f <- pmax((0:(nk-1)) * sr / n, sr/n)
    x <- complex(modulus=1/sqrt(f), argument=stats::runif(nk, 0, 2*pi))
    y <- Re(stats::fft(c(x, Conj(rev(x[2:(nk-1)]))), inverse=TRUE))
    pink <- tuneR::Wave(round(y / stats::sd(y) * 0.05 * 32767),
                        samp.rate=sr, bit=16, pcm=TRUE)
    expect_equal(rainfallDetection(pink), 0)
  }
  # Near silence is not rain either: noise of a few least significant bits used
  # to clear both thresholds.
  set.seed(11)
  faint <- tuneR::Wave(round(rnorm(2*sr, sd=1e-4) * 32767),
                       samp.rate=sr, bit=16, pcm=TRUE)
  expect_equal(rainfallDetection(faint), 0)
})

test_that("rainfallDetection refuses a band too narrow to measure, saying why", {
  # The ratio is taken across the band's bins, each the sample rate over 512
  # wide, and at the 384 and 250 kHz of bat detectors the 600-1200 Hz band holds
  # one, which has no standard deviation. It printed a message and gave NULL,
  # which a pipeline stores as a step with nothing to report, so recordings it
  # could not measure were stored as measured, with no row to say otherwise.
  noise <- function(sr) {
    set.seed(13)
    tuneR::Wave(round(rnorm(sr %/% 10, sd=0.1) * 32767), samp.rate=sr, bit=16, pcm=TRUE)
  }
  expect_error(
    rainfallDetection(noise(384000)),
    paste("cannot be applied at a sample rate of 384000 Hz. Its 600-1200 Hz band holds",
          "1 frequency bin of the 512-sample windows it uses, which are 750 Hz wide"),
    fixed=TRUE
  )
  expect_error(rainfallDetection(noise(250000)),
               "holds 1 frequency bin of the 512-sample windows it uses, which are 488.3 Hz wide",
               fixed=TRUE)
  expect_no_message(try(rainfallDetection(noise(384000)), silent=TRUE))
  # A bin wider than the band leaves it none, and a rate this high is written in
  # full rather than as 1e+06.
  expect_error(rainfallDetection(noise(1e6)),
               "sample rate of 1000000 Hz. Its 600-1200 Hz band holds 0 frequency bins",
               fixed=TRUE)
  # At 200 kHz the band still holds two, and at the 48 kHz the message offers, six.
  expect_type(rainfallDetection(noise(200000)), "double")
  expect_type(rainfallDetection(noise(48000)), "double")
  # Each channel is refused as a mono wave is.
  w <- noise(384000)
  expect_error(rainfallDetection(tuneR::stereo(w, w)), "384000 Hz", fixed=TRUE)
})

test_that("rainfallDetection refuses a wave sampled too slowly to reach the band", {
  set.seed(14)
  w <- tuneR::Wave(round(rnorm(1000, sd=0.1) * 32767), samp.rate=1000, bit=16, pcm=TRUE)
  expect_error(
    rainfallDetection(w),
    "measures the 600-1200 Hz band, and a wave sampled at 1000 Hz reaches only 500 Hz.",
    fixed=TRUE
  )
})

test_that("rainfall is still detected", {
  sr <- 22050
  set.seed(12)
  # Rain is loud broadband noise in this band, so noise at a level rain reaches
  # has to be reported: rejecting quiet noise must not come from rejecting
  # everything. -14 dBFS is about where the published threshold starts to pass
  # broadband noise.
  loud <- tuneR::Wave(round(rnorm(2*sr, sd=0.3) * 32767),
                      samp.rate=sr, bit=16, pcm=TRUE)
  expect_gt(rainfallDetection(loud), 0)
  # A tone is not rain whatever its level, because the band is not flat.
  t <- (1:(2*sr))/sr
  tone <- tuneR::Wave(round(sin(2*pi*800*t) * 0.8 * 32767),
                      samp.rate=sr, bit=16, pcm=TRUE)
  expect_equal(rainfallDetection(tone), 0)
})
