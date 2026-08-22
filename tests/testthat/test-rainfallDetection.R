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
