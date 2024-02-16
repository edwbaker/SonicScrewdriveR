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
