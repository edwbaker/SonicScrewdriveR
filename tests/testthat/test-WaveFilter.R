test_that("filterWave rejects unknown inputs", {
  # Generate list of Wave objects
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    1
  )

  # Use return() to make no change
  filter <- new(
    "WaveFilter",
    description="Do absolutely nowt"
  )
  # Apply the filter to the Wave objects
  expect_error(filterWave(waves, filter), "Can only filter a Wave or WaveMC object.")
})

test_that("output is the same when filter is nothing (TaggedWave)", {
  # Generate list of Wave objects
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )
  # Tag the Wave objects
  waves <- tagWave(waves)

  filter <- new(
    "WaveFilter",
    description="Do absolutely nowt"
  )
  # Apply the filter to the Wave objects
  filtered <- filterWave(waves, filter)
  expect_equal(length(waves), length(filtered))
  expect_true(all(sapply(filtered, inherits, what=c("TaggedWave"))))

  # Check description is added
  expect_equal(typeof(filtered[[1]]@processing[[1]]), "list")
  expect_equal(length(filtered[[1]]@processing[[1]]), 2)
  expect_equal(filtered[[1]]@processing[[1]]$process, "Do absolutely nowt")
})

test_that("output is the same when filter is nothing (Wave)", {
  # Generate list of Wave objects
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )

  filter <- new(
    "WaveFilter",
    description="Do absolutely nowt"
  )
  # Apply the filter to the Wave objects
  filtered <- filterWave(waves, filter)
  expect_equal(length(waves), length(filtered))
  expect_true(all(sapply(filtered, inherits, what=c("Wave"))))
  expect_true(all(sapply(filtered, is, "Wave")))
})

test_that("bandpass filter works as expected", {
  expect_true(is(bandpass(1000,2000), "WaveFilter"))

  fw <- noise("white", duration=44100, samp.rate=44100) |> filterWave(bandpass(1000,2000))
  ss <- seewave::spec(fw, plot=FALSE)
  expect_true(all(ss[ss[,1] < 0.9, 2] < 0.01))
  expect_true(all(ss[ss[,1] >= 2.1, 2] < 0.01))
})

test_that("filterWave() works with cluster", {
  if (.Platform$OS.type == "windows") {
    return()
  }

  cl <- makeForkCluster(2, outfile="")

  # Generate list of Wave objects
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )
  # Tag the Wave objects
  waves <- tagWave(waves)

  filter <- new(
    "WaveFilter",
    description="Do absolutely nowt"
  )
  # Apply the filter to the Wave objects
  filtered <- filterWave(waves, filter, cl=cl)
  expect_equal(length(waves), length(filtered))
  expect_true(all(sapply(filtered, inherits, what=c("TaggedWave"))))

  # Check description is added
  expect_equal(typeof(filtered[[1]]@processing[[1]]), "list")
  expect_equal(length(filtered[[1]]@processing[[1]]), 2)
  expect_equal(filtered[[1]]@processing[[1]]$process, "Do absolutely nowt")

  parallel::stopCluster(cl)
})
