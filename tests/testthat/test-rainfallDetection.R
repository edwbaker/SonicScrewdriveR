test_that("rainfallDetection analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  quiet <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  rain <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)

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
  w <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)
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
