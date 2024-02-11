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
  m <- tuneR::sine(440, duration=44100, samp.rate=44100)
  s <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  pw <- pseudoWave("noise", "white")
  mpw <- m + pw
  spw <- s + pw
  expect_true(inherits(mpw, "Wave"))
  expect_true(inherits(spw, "Wave"))
  expect_equal(length(mpw@left), 44100)
  expect_equal(length(spw@left), 44100)
  expect_equal(length(spw@right), 44100)
})
