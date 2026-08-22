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

  rmpw <- pw + m
  rspw <- pw + s

  # Should return a Wave object
  expect_true(inherits(mpw, "Wave"))
  expect_true(inherits(spw, "Wave"))
  expect_true(inherits(rmpw, "Wave"))
  expect_true(inherits(rspw, "Wave"))
  expect_equal(length(mpw@left), 44100)
  expect_equal(length(spw@left), 44100)
  expect_equal(length(rmpw@left), 44100)
  expect_equal(length(spw@right), 44100)
  expect_equal(length(rspw@right), 44100)

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

  # Generate sum of silence and sine PseudoWave
  spw1 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440))
  expect_lte(max(spw1@left), 1)
  expect_lte(mean(spw1@left), 0.001)

  # Test scale parameter and multiplication / division
  spw2 <- tuneR::silence(duration=44100) + pseudoWave("sine", scale=0.5, params=list(f0=440))
  expect_lte(max(spw2@left), 0.5)
  expect_lte(mean(spw2@left), 0.001)

  spw4 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440)) * 0.5
  expect_lte(max(spw4@left), 0.5)
  expect_lte(mean(spw4@left), 0.001)

  spw6 <- tuneR::silence(duration=44100) + 0.5 * pseudoWave("sine", params=list(f0=440))
  expect_lte(max(spw4@left), 0.5)
  expect_lte(mean(spw4@left), 0.001)

  spw8 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440)) / 2
  expect_lte(max(spw8@left), 0.5)
  expect_lte(mean(spw8@left), 0.001)

  # Test offset parameter and addition / subtraction
  spw3 <- tuneR::silence(duration=44100) + pseudoWave("sine", offset=1, params=list(f0=440))
  expect_lte(max(spw3@left), 2)
  expect_gte(min(spw3@left), 0)
  expect_lte(mean(spw3@left), 1.001)
  expect_gte(mean(spw3@left), 0.999)

  spw5 <- tuneR::silence(duration=44100) + (pseudoWave("sine", params=list(f0=440)) + 1)
  expect_lte(max(spw5@left), 2)
  expect_gte(min(spw5@left), 0)
  expect_lte(mean(spw5@left), 1.001)
  expect_gte(mean(spw5@left), 0.999)

  spw7 <- tuneR::silence(duration=44100) + (1 + pseudoWave("sine", params=list(f0=440)))
  expect_lte(max(spw7@left), 2)
  expect_gte(min(spw7@left), 0)
  expect_lte(mean(spw7@left), 1.001)
  expect_gte(mean(spw7@left), 0.999)

  spw9 <- tuneR::silence(duration=44100) + (pseudoWave("sine", params=list(f0=440)) - 0.50)
  expect_lte(max(spw9@left), 0.5)
  expect_lte(mean(spw9@left), 0.001)
})

test_that("pseudoWave is reproducible by default", {
  # The default seed is what makes the same PseudoWave give the same samples each
  # time. The class prototype used to disagree with the constructor about it.
  expect_equal(eval(formals(pseudoWave)$seed), 1)
  expect_equal(pseudoWave("noise", "white")@seed, new("PseudoWave")@seed)

  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_equal(
    (m + pseudoWave("noise", "white"))@left,
    (m + pseudoWave("noise", "white"))@left
  )
})

test_that("a seed of NA asks for a fresh sequence each time", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  # A bare NA is logical, and the slot holds a number, so this used to be
  # rejected by the class.
  for (none in list(NA, NA_integer_, NA_real_)) {
    expect_false(identical(
      (m + pseudoWave("noise", "white", seed=none))@left,
      (m + pseudoWave("noise", "white", seed=none))@left
    ))
  }
})

test_that("pseudoWave takes one seed", {
  expect_error(pseudoWave("noise", "white", seed=c(1, 2)), "a single seed")
})

test_that("different seeds give different noise", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_false(identical(
    (m + pseudoWave("noise", "white", seed=5))@left,
    (m + pseudoWave("noise", "white", seed=6))@left
  ))
})

test_that("an explicit seed still gives reproducible noise", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_equal(
    (m + pseudoWave("noise", "white", seed=5))@left,
    (m + pseudoWave("noise", "white", seed=5))@left
  )
})

test_that("a seeded PseudoWave leaves the caller's random numbers alone", {
  # set.seed() replaces the random stream of the whole session, so every random
  # number drawn after a seeded PseudoWave used to change.
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)

  set.seed(99)
  before <- runif(3)

  set.seed(99)
  invisible(m + pseudoWave("noise", "white", seed=5))
  after <- runif(3)

  expect_equal(before, after)
})

test_that("a file PseudoWave gives as many samples as the wave it meets", {
  # The file was read whole and then recycled against the target by the
  # arithmetic, whatever the two lengths were.
  f <- tempfile(fileext=".wav")
  tuneR::writeWave(tuneR::sine(300, duration=500, samp.rate=44100, bit=16, pcm=TRUE), f)
  pw <- pseudoWave("file", f)

  for (n in c(200, 500, 1300)) {
    target <- tuneR::sine(440, duration=n, samp.rate=44100, bit=16, pcm=TRUE)
    expect_equal(length((target + pw)@left), n)
  }
})

test_that("pseudoWave rejects a type it cannot generate", {
  # An unknown type left the wave unassigned, so the arithmetic failed with
  # "object 'w' not found".
  expect_error(pseudoWave("dog"), "Unsupported PseudoWave type: dog")
  expect_error(
    depseduoWave(new("PseudoWave", type="dog"), 10, FALSE, 44100, 16, TRUE),
    "Unsupported PseudoWave type: dog"
  )
})
