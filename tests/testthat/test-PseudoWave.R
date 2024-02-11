test_that("PseudoWave creation", {
  expect_error(inherits(pseudoWave(), "Either type or url must be specified"))
  expect_true(inherits(pseudoWave("noise", "white"), "PseudoWave"))
  expect_equal(pseudoWave("noise", "white")@type, "noise")
  expect_equal(pseudoWave("noise", "white")@subtype, "white")
  expect_equal(pseudoWave("noise", "white", scale=2)@scale, 2)
  expect_equal(pseudoWave("noise", "white", offset=3)@offset, 3)
  expect_equal(pseudoWave("noise", "white", seed=5)@seed, 5)
})

test_that("depseduoWave tests", {
  # Generate mono and stero sine waves
  m <- tuneR::sine(440, duration=44100, samp.rate=44100)
  s <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)

  # Generate a white noise PseudoWave
  pw <- pseudoWave("noise", "white")

  # Add PseudoWave to sine
  mpw <- m + pw
  spw <- s + pw

  # Should return a Wave object
  expect_true(inherits(mpw, "Wave"))
  expect_true(inherits(spw, "Wave"))
  expect_equal(length(mpw@left), 44100)
  expect_equal(length(spw@left), 44100)
  expect_equal(length(spw@right), 44100)

  # Generate mono and stereo silence
  mq <- tuneR::silence(44100, samp.rate=44100)
  sq <- tuneR::silence(44100, samp.rate=44100, stereo=TRUE)

  # Multiply PseudoWave by silence
  mqw <- mq * pw
  sqw <- sq * pw

  # Should return a silent Wave object
  expect_true(inherits(mqw, "Wave"))
  expect_true(inherits(sqw, "Wave"))
  expect_equal(sum(mqw@left), 0)
  expect_equal(sum(sqw@right), 0)
})
