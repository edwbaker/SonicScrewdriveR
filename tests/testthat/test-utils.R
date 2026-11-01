test_that(".equalWave gives correct output", {
  w1 <- tuneR::sine(440, duration=44100, samp.rate=44100)
  w2 <- tuneR::silence(440, duration=44100, samp.rate=44100)

  d1 <- tuneR::noise("white", duration=44100, samp.rate=44100, stereo=T)
  d2 <- tuneR::sawtooth(440, duration=44100, samp.rate=44100, stereo=T)

  expect_silent(.equalWave(w1, w1))
  expect_silent(.equalWave(w1, w2))
  expect_error(.equalWave(w1, d1))
  expect_silent(.equalWave(d1, d2))

  # ToDo: WaveMC, & Tagged
})

test_that(".setPCM gives correct output", {
  expect_true(.setPCM(1, TRUE))
  expect_false(.setPCM(1, FALSE))
  expect_true(.setPCM(8, TRUE))
  expect_warning(.setPCM(8, FALSE))
  expect_true(.setPCM(16, TRUE))
  expect_warning(.setPCM(16, FALSE))
  expect_true(.setPCM(24, TRUE))
  expect_warning(.setPCM(24, FALSE))
  expect_true(.setPCM(32, TRUE))
  expect_false(.setPCM(32, FALSE))
  expect_warning(.setPCM(64, TRUE))
  expect_false(.setPCM(64, FALSE))
  expect_error(.setPCM(7, TRUE), "bit must be one of 8, 16, 24, 32, or 64.")
})

test_that(".rebuildWave takes its class from the samples and the wave", {
  mono <- tuneR::Wave(c(1, 2, 3), samp.rate=8000, bit=16)
  st <- tuneR::Wave(c(1, 2, 3), right=c(4, 5, 6), samp.rate=8000, bit=16)
  mc <- tuneR::WaveMC(cbind(A=c(1, 2, 3), B=c(4, 5, 6)), samp.rate=8000, bit=16)

  # A vector is one channel, whatever it was taken from.
  expect_false(.rebuildWave(c(7, 8), mono)@stereo)
  one <- .rebuildWave(c(7, 8), mc)
  expect_s4_class(one, "Wave")
  expect_false(one@stereo)
  expect_equal(one@left, c(7, 8))

  # A matrix keeps the class of the wave, so a stereo Wave stays one.
  two <- .rebuildWave(cbind(c(7, 8), c(9, 10)), st)
  expect_s4_class(two, "Wave")
  expect_true(two@stereo)
  expect_equal(two@left, c(7, 8))
  expect_equal(two@right, c(9, 10))
  expect_false(.rebuildWave(cbind(c(7, 8)), mono)@stereo)

  many <- .rebuildWave(mc@.Data[2:3, , drop=FALSE], mc)
  expect_s4_class(many, "WaveMC")
  expect_equal(colnames(many@.Data), c("A", "B"))
})

test_that(".rebuildWave declares the format of the wave it is given", {
  formats <- list(
    list(bit=8, pcm=TRUE), list(bit=16, pcm=TRUE), list(bit=24, pcm=TRUE),
    list(bit=32, pcm=TRUE), list(bit=32, pcm=FALSE), list(bit=64, pcm=FALSE)
  )
  for (format in formats) {
    w <- tuneR::Wave(c(0, 1), samp.rate=8000, bit=format$bit, pcm=format$pcm)
    rebuilt <- .rebuildWave(c(0, 1, 0), w)
    expect_equal(rebuilt@bit, format$bit)
    expect_equal(rebuilt@pcm, format$pcm)
    expect_equal(rebuilt@samp.rate, 8000)
  }
})

test_that(".rebuildWave rounds samples where the format is integer PCM", {
  pcm <- tuneR::Wave(c(0, 1), samp.rate=8000, bit=16)
  float <- tuneR::Wave(c(0, 1), samp.rate=8000, bit=32, pcm=FALSE)
  expect_equal(.rebuildWave(c(0.4, 1.6, -2.7), pcm)@left, c(0, 2, -3))
  expect_equal(.rebuildWave(c(0.4, 1.6, -2.7), float)@left, c(0.4, 1.6, -2.7))

  mc <- tuneR::WaveMC(cbind(c(0, 1), c(0, 1)), samp.rate=8000, bit=16)
  rebuilt <- .rebuildWave(cbind(c(0.4, 1.6), c(2.2, -0.7)), mc)
  expect_equal(unname(rebuilt@.Data), cbind(c(0, 2), c(2, -1)))
})

test_that(".rebuildWave leaves integer samples as integers", {
  # round() returns doubles, so rounding samples that were whole already would
  # change the type of the samples of a wave read from a file.
  w <- tuneR::Wave(c(1L, 2L, 3L), samp.rate=8000, bit=16)
  expect_identical(.rebuildWave(w@left, w)@left, c(1L, 2L, 3L))
})

test_that(".rebuildWave takes a sample rate, bit and pcm in place of the wave's", {
  w <- tuneR::Wave(c(0, 1), samp.rate=8000, bit=16)
  expect_equal(.rebuildWave(c(0, 1), w, samp.rate=16000)@samp.rate, 16000)

  # Rounding follows the format asked for rather than the one the wave has.
  float <- .rebuildWave(c(0.25, 0.75), w, bit=32, pcm=FALSE)
  expect_false(float@pcm)
  expect_equal(float@left, c(0.25, 0.75))

  f <- tuneR::Wave(c(0, 0.5), samp.rate=8000, bit=32, pcm=FALSE)
  pcm <- .rebuildWave(c(0.25, 0.75), f, bit=16, pcm=TRUE)
  expect_true(pcm@pcm)
  expect_equal(pcm@bit, 16)
  expect_equal(pcm@left, c(0, 1))
})
