test_that("output is the same", {
  # Generate list of Wave objects
  waves <- list(
    tuneR::silence(duration=44100, samp.rate=44100),
    tuneR::noise(kind="white", duration=44100, samp.rate=44100),
    tuneR::sine(440, duration=44100, samp.rate=44100)
  )
  # Tag the Wave objects
  waves <- tagWave(waves)

  # Use return() to make no change
  filter <- new(
    "WaveFilter",
    description="Do absolutely nowt",
    func="doNowt"
  )
  # Apply the filter to the Wave objects
  filtered <- filterWave(waves, filter)
  expect_equal(length(waves), length(filtered))
  expect_true(all(sapply(filtered, inherits, what=c("TaggedWave"))))

  # Check description is added
  expect_equal(typeof(filtered[[1]]@processing), "list")
  expect_equal(length(filtered[[1]]@processing), 1)
  expect_equal(filtered[[1]]@processing[[1]], "Do absolutely nowt")
})
