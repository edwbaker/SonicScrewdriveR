test_that("generateNoise rejects unknown input to wave", {
  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_silent(generateNoise(w, "white", FALSE, 0.5, "list"))

  w <- tagWave(w)
  expect_silent(generateNoise(w, "white", FALSE, 0.5, "list"))

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100)),
    "koi carp"
  )
  expect_error(generateNoise(w, "white", FALSE, 0.5, "list"), "wave must be a Wave like object, or a list of such objects.")
})

test_that("generateNoise outputs somethign Wave-like", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, "list"), "Wave")

  w <- tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, "list"), "WaveMC")

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_type(generateNoise(w, "white", FALSE, 0.5, "list"), "list")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, "list")[[1]], "Wave")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, "list")[[2]], "WaveMC")
})
