test_that("Correct value is output", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="unit", normalise=FALSE), 0.5)
  d <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_equal(dutyCycle(d, output="unit", normalise=TRUE, limit=0.5), 2/3)
})

test_that("Corect value is output in percantage mode", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="percent", normalise=FALSE), 50)
})

test_that("Multi-channel input is analysed one channel at a time", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1500*(1:n)/sr) * 3000), samp.rate=sr, bit=16)

  # The single channel answers the multi-channel calls must reproduce.
  # allChannels() wraps each channel's result in a list of its own.
  left <- dutyCycle(L)
  right <- dutyCycle(R)
  expect_false(isTRUE(all.equal(left, right)))

  expect_equal(dutyCycle(tuneR::stereo(L, R)), list(list(left), list(right)))
  expect_equal(
    dutyCycle(tuneR::WaveMC(cbind(L@left, R@left, L@left), samp.rate=sr, bit=16)),
    list(list(left), list(right), list(left))
  )
})

test_that("Arguments are passed through to each channel", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1500*(1:n)/sr) * 3000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, R)

  expect_equal(unlist(dutyCycle(st, output="percent")),
               c(dutyCycle(L, output="percent"), dutyCycle(R, output="percent")))
  expect_equal(unlist(dutyCycle(st, limit=0.5)),
               c(dutyCycle(L, limit=0.5), dutyCycle(R, limit=0.5)))
  expect_equal(unlist(dutyCycle(st, normalise=FALSE)),
               c(dutyCycle(L, normalise=FALSE), dutyCycle(R, normalise=FALSE)))
})

test_that("a wave holding NA samples is refused by name", {
  # The NA reached validateDutyCycle() as an NA duty cycle and came back as
  # "missing value where TRUE/FALSE needed", naming neither NA nor the wave.
  # The NA goes into the slot rather than through Wave(), so the test is of
  # dutyCycle() and not of tuneR's validity checking.
  w <- tuneR::Wave(c(rep.int(1, 500), rep.int(0, 500)), samp.rate=8000, bit=16)
  w@left[501] <- NA

  expect_error(dutyCycle(w, normalise=FALSE), "NA samples")
  expect_error(dutyCycle(w, normalise=TRUE), "NA samples")
  expect_error(dutyCycle(w, output="percent", normalise=FALSE), "NA samples")
})
