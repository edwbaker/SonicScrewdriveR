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
