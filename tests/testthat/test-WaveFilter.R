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
  expect_true(all(c("process", "output", "func", "time") %in% names(filtered[[1]]@processing[[1]])))
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

test_that("filterWave records the function and arguments of a named filter", {
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  tw@metadata <- list(site = "here")
  fw <- filterWave(tw, bandpass(1000, 2000))
  expect_s4_class(fw, "TaggedWave")
  expect_equal(fw@metadata$site, "here")
  rec <- fw@processing[[1]]
  expect_equal(rec$process, "bandpass 1000-2000 Hz")
  expect_equal(rec$func, "seewave::ffilter")
  expect_equal(rec$params$from, 1000)
  expect_equal(rec$params$to, 2000)
  expect_equal(rec$input_hash, .waveHash(tw))
  expect_equal(rec$output_hash, .waveHash(fw))
})

test_that("a filter without a name is recorded as an anonymous function", {
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  filt <- new("WaveFilter", description="Do absolutely nowt")
  expect_true(is.na(filt@func_name))
  expect_equal(filterWave(tw, filt)@processing[[1]]$func, "<anonymous function>")
})

test_that("a filter can be rebuilt from its record", {
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  fw <- filterWave(tw, bandpass(1000, 2000))

  filt <- .filterFromRecord(fw@processing[[1]])
  expect_true(is(filt, "WaveFilter"))
  expect_equal(filt@func_name, "seewave::ffilter")
  expect_equal(filt@description, "bandpass 1000-2000 Hz")

  # applying the rebuilt filter gives the same result
  expect_equal(.waveHash(filterWave(tw, filt)), .waveHash(fw))

  expect_error(.filterFromRecord(list(process="x", func="<anonymous function>")), "cannot be rebuilt")
  expect_error(.filterFromRecord(list(process="x")), "cannot be rebuilt")
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
  expect_true(all(c("process", "output", "func", "time") %in% names(filtered[[1]]@processing[[1]])))
  expect_equal(filtered[[1]]@processing[[1]]$process, "Do absolutely nowt")

  parallel::stopCluster(cl)
})
