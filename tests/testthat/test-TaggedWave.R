test_that(".tagSlots returns correct format", {
  expect_equal(typeof(.tagSlots()), "list")
})

test_that("attempt to convert non-Wave like object throws error", {
  expect_error(tagWave(1), "Attempting to tag object that is not of type Wave or WaveMC.")
})

test_that("converting Wave to TaggedWave works", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  tw <- tagWave(w)
  expect_true(inherits(tw, "Wave"))
  expect_true(is(tw, "TaggedWave"))
  expect_false(inherits(tw, "WaveMC"))

  # Check converting already tagged
  tw <- tagWave(tw)
  expect_true(inherits(tw, "Wave"))
  expect_true(is(tw, "TaggedWave"))
  expect_false(inherits(tw, "WaveMC"))

  utw <- untagWave(tw)
  expect_true(inherits(utw, "Wave"))
  expect_false(is(utw, "TaggedWave"))
  expect_false(inherits(utw, "TaggedWave"))

  expect_equal(w@left, utw@left)
})

test_that("converting WaveMC to TaggedWaveMC works", {
  x <- seq(0, 2*pi, length = 44100)
  channel <- round(32000 * sin(440 * x))
  w <- tuneR::WaveMC(data = channel, samp.rate = 44100, bit = 16)
  tw <- tagWave(w)
  expect_true(inherits(tw, "WaveMC"))
  expect_true(is(tw, "TaggedWaveMC"))
  expect_false(inherits(tw, "Wave"))

  # Check converting already tagged
  tw <- tagWave(tw)
  expect_true(inherits(tw, "WaveMC"))
  expect_true(is(tw, "TaggedWaveMC"))
  expect_false(inherits(tw, "Wave"))

  utw <- untagWave(tw)
  expect_true(inherits(utw, "WaveMC"))
  expect_false(is(utw, "TaggedWaveMC"))
  expect_false(inherits(utw, "TaggedWaveMC"))
  expect_equal(dim(w@`.Data`), dim(utw@`.Data`))
  expect_equal(w@`.Data`, utw@`.Data`)
})

test_that("setting origin works", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)

  tw <- tagWave(w)
  expect_equal(tw@origin, "user")

  tw <- tagWave(w, origin="test")
  expect_equal(tw@origin, "test")
})

test_that("list of waves works", {
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )

  tws <- tagWave(waves)
  expect_true(all(sapply(tws, inherits, what=c("TaggedWave"))))

  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    14,
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )
  expect_error(tagWave(waves), "All items in list must be Wave or WaveMC objects.")
})
