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
  expect_equal(filtered[[1]]@processing[[1]]$process, "Do absolutely nowt")
  # A record is no longer a process and an output alone: what was done, with
  # what, under which versions, when and to which audio.
  expect_named(
    filtered[[1]]@processing[[1]],
    c("process", "output", "func", "versions", "params", "time", "timezone",
      "duration", "hash_method", "input_hash", "output_hash")
  )
})

test_that("a filter's record says which function filtered", {
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  own <- getNamespaceVersion(asNamespace("sonicscrewdriver"))[[1]]

  # The settings alone did not say what was done with them: two filters given
  # the same cut-offs were recorded alike.
  rec <- filterWave(tw, bandpass(from=1000, to=2000))@processing[[1]]
  expect_equal(rec$process, "filterWave")
  expect_equal(rec$func, "filterWave")
  expect_equal(
    rec$params,
    list(filter="sonicscrewdriver:::.ffilterWave", from=1000, to=2000)
  )
  expect_null(rec$defaults)
  expect_equal(rec$versions, c(sonicscrewdriver=own))

  # A filter built on another package's function is described as that function
  # describes it: its settings named as it names them, given by position or
  # not, the arguments it chose for itself, and its package's version.
  scaled <- new("WaveFilter", func=tuneR::normalize, params=list("1"))
  rec <- filterWave(tw, scaled)@processing[[1]]
  expect_equal(rec$func, "filterWave")
  expect_equal(rec$params, list(filter="tuneR::normalize", unit="1"))
  expect_equal(rec$defaults$center, "TRUE")
  expect_equal(rec$defaults$pcm, "object@pcm")
  expect_equal(
    rec$versions,
    c(tuneR=getNamespaceVersion(asNamespace("tuneR"))[[1]], sonicscrewdriver=own)
  )

  # A filter written inline has no name to record, but is still told apart
  # from one that has.
  inline <- new("WaveFilter", func=function(x) x, description="as it was")
  rec <- filterWave(tw, inline)@processing[[1]]
  expect_equal(rec$process, "as it was")
  expect_equal(rec$params, list(filter="<anonymous function>"))

  # One written at the top level goes by the name it was given there.
  topLevel <- function(x) x
  environment(topLevel) <- globalenv()
  assign(".ssdTopLevelFilter", topLevel, envir=globalenv())
  withr::defer(rm(".ssdTopLevelFilter", envir=globalenv()))
  rec <- filterWave(tw, new("WaveFilter", func=topLevel))@processing[[1]]
  expect_equal(rec$params$filter, ".ssdTopLevelFilter")
})

test_that("a filter is named once, by the caller, not by each wave", {
  # The global environment a top-level filter is found in is not shared by the
  # workers of a cluster, which would each record it as anonymous. The name is
  # found once and handed to every wave, whichever process filters it.
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  filt <- new("WaveFilter", func=function(x) x)
  done <- .filterWave(list(tw, tw), filt, NULL, "as the caller found it")
  for (w in done) {
    expect_equal(w@processing[[1]]$params$filter, "as the caller found it")
  }
  # And it is not looked for where no record is made.
  plain <- .filterWave(untagWave(tw), filt, NULL, stop("looked for"))
  expect_false(.isTagged(plain))

  # A cluster is handed the arguments as they stand in the caller, and its
  # workers do not see the caller's global environment. Stood in for here by a
  # worker that runs once the filter has gone from it.
  topLevel <- function(x) x
  environment(topLevel) <- globalenv()
  assign(".ssdClusterFilter", topLevel, envir=globalenv())
  withr::defer(if (exists(".ssdClusterFilter", envir=globalenv())) {
    rm(".ssdClusterFilter", envir=globalenv())
  })
  testthat::local_mocked_bindings(
    parLapply=function(cl, X, fun, ...) {
      args <- list(...)
      rm(".ssdClusterFilter", envir=globalenv())
      lapply(X, function(x) do.call(fun, c(list(x), args)))
    },
    .package="parallel"
  )
  done <- filterWave(list(tw, tw), new("WaveFilter", func=topLevel), cl="workers")
  for (w in done) {
    expect_equal(w@processing[[1]]$params$filter, ".ssdClusterFilter")
  }
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
  expect_equal(filtered[[1]]@processing[[1]]$process, "Do absolutely nowt")
  # A record is no longer a process and an output alone: what was done, with
  # what, under which versions, when and to which audio.
  expect_named(
    filtered[[1]]@processing[[1]],
    c("process", "output", "func", "versions", "params", "time", "timezone",
      "duration", "hash_method", "input_hash", "output_hash")
  )

  parallel::stopCluster(cl)
})
