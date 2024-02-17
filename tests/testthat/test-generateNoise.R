test_that("generateNoise rejects unknown input to wave", {
  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_silent(generateNoise(w, "white", FALSE, 0.5, output="list"))

  w <- tagWave(w)
  expect_silent(generateNoise(w, "white", FALSE, 0.5, output="list"))

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100)),
    "koi carp"
  )
  expect_error(generateNoise(w, "white", FALSE, 0.5, output="list"), "wave must be a Wave like object, or a list of such objects.")
})

test_that("generateNoise outputs somethign Wave-like", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list"), "Wave")

  w <- tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list"), "WaveMC")

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_type(generateNoise(w, "white", FALSE, 0.5, output="list"), "list")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list")[[1]], "Wave")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list")[[2]], "WaveMC")
})

test_that("noise amplitude is correct on Wave", {
  w <- pulse("dirac", leading=0)
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")

  expect_equal(length(w), length(n))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
})

test_that("noise amplitude is correct on TaggedWave", {
  w <- pulse("dirac", leading=0, output="TaggedWave")
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@left))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)

  w <- pulse("dirac", leading=0, output="TaggedWave", stereo=TRUE)
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@right))
  expect_equal(length(w@left), length(n@left))
  expect_equal(length(w@right), length(n@right))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_equal(which(n@right > max(abs(n@left))/2), 1)
})
