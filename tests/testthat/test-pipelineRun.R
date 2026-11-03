.runWave <- function(samples=4410) {
  tagWave(noise("white", duration=samples, samp.rate=44100))
}

# The function each record in a history is under
.historyOf <- function(w) {
  vapply(w@processing, `[[`, character(1), "func")
}

test_that("a wave goes on to the steps after it, and an analysis keeps the wave", {
  tw <- .runWave()
  half <- function(w) cutws(w, 1, length(w) / 2)
  ran <- .runPipelineWave(pipeline(normalise(), dutyCycle(), half(), length()), tw)

  expect_named(ran$results, c("dutyCycle()", "length()"))
  expect_equal(ran$results[["length()"]], data.frame(value = 2205L))
  expect_equal(
    ran$results[["dutyCycle()"]]$value,
    dutyCycle(normalise(tw))
  )
  expect_equal(.historyOf(ran$wave), c("normalise", "dutyCycle", "half", "length"))
  expect_equal(length(ran$wave), 2205)
})

test_that("an analysis puts its result in its record, which keeps the digest of the wave", {
  tw <- .runWave()
  ran <- .runPipelineWave(pipeline(dutyCycle(limit = 0.2)), tw)
  rec <- ran$wave@processing[[1]]
  expect_named(rec, c(
    "process", "output", "func", "versions", "params", "defaults", "time",
    "timezone", "duration", "hash_method", "input_hash", "output_hash"
  ))
  expect_equal(rec$output, dutyCycle(tw, limit = 0.2))
  expect_equal(rec$params, list(limit = 0.2))
  expect_equal(rec$input_hash, .waveHash(tw))
  expect_equal(rec$output_hash, rec$input_hash)

  # A result that is NULL is still recorded, with an empty table.
  ran <- .runPipelineWave(pipeline((function(w) NULL)()), tw)
  expect_equal(nrow(ran$results[[1]]), 0)
  rec <- ran$wave@processing[[1]]
  expect_true("output" %in% names(rec))
  expect_null(rec$output)
  expect_equal(rec$output_hash, rec$input_hash)

  # The history tagApply() attaches to a result is not part of it.
  ran <- .runPipelineWave(pipeline(tagApply(dutyCycle)), tw)
  expect_null(attr(ran$wave@processing[[1]]$output, "processing"))
  # An environment is not copied when changed, so it is kept as it is.
  box <- new.env()
  attr(box, "processing") <- "kept"
  ran <- .runPipelineWave(pipeline((function(w) box)()), tw)
  expect_identical(ran$wave@processing[[1]]$output, box)
  expect_equal(attr(box, "processing"), "kept")
})

test_that("then= gives the new wave to its pipeline and leaves the rest the old one", {
  tw <- .runWave()
  p <- pipeline(
    cutws(1, 1000, then = pipeline(length(), cutws(1, 10, then = length()))),
    length()
  )
  ran <- .runPipelineWave(p, tw)
  expect_named(ran$results, c(
    "cutws(1, 1000) / length()",
    "cutws(1, 1000) / cutws(1, 10) / length()",
    "length()"
  ))
  expect_equal(unlist(lapply(ran$results, `[[`, "value")), c(1000, 10, 4410), ignore_attr = TRUE)
  # The cut is not in the history of the wave the pipeline carried on.
  expect_equal(.historyOf(ran$wave), "length")
})

test_that("two steps with the same path have their numbers added", {
  ran <- .runPipelineWave(
    pipeline(length(), cutws(1, 10), length(), cutws(1, 5, then = length())),
    .runWave()
  )
  expect_named(ran$results, c("length() [1]", "length() [3]", "cutws(1, 5) / length()"))
  expect_equal(unname(unlist(lapply(ran$results, `[[`, "value"))), c(4410, 10, 5))
})

test_that("a function of this package keeps the record it makes itself", {
  tw <- .runWave()
  ran <- .runPipelineWave(pipeline(filterWave(bandpass(2000, 11000))), tw)
  direct <- filterWave(tw, bandpass(2000, 11000))
  rec <- ran$wave@processing[[1]]
  expect_equal(rec$params, direct@processing[[1]]$params)
  expect_equal(rec$params$filter, "sonicscrewdriver:::.ffilterWave")
  expect_equal(rec$output_hash, direct@processing[[1]]$output_hash)
  expect_length(ran$wave@processing, 1)

  # A function of this package that gives a wave without recording it is
  # recorded as tagApply() records it.
  ran <- .runPipelineWave(pipeline(tagWave(), untagWave()), tw)
  expect_equal(.historyOf(ran$wave), c("tagWave", "untagWave"))

  # A user's function is one record, even where it calls one that records
  # itself.
  mine <- function(w) cutws(normalise(w), 1, 100)
  ran <- .runPipelineWave(pipeline(mine()), tw)
  expect_equal(.historyOf(ran$wave), "mine")
  expect_equal(length(ran$wave), 100)

  # Another package's function is given the wave untagged, and its result is
  # given the tags back.
  ran <- .runPipelineWave(pipeline(seewave::ffilter(from = 1000, output = "Wave")), tw)
  expect_s4_class(ran$wave, "TaggedWave")
  expect_equal(.historyOf(ran$wave), "seewave::ffilter")
  expect_equal(ran$wave@processing[[1]]$params, list(from = 1000, output = "Wave"))
})

test_that("a wave without tags gives the same results and keeps no history", {
  w <- untagWave(.runWave())
  ran <- .runPipelineWave(pipeline(normalise(), dutyCycle(), cutws(1, 9, then = length())), w)
  expect_false(.isTagged(ran$wave))
  expect_equal(ran$results[["dutyCycle()"]]$value, dutyCycle(normalise(w)))
  expect_equal(ran$results[["cutws(1, 9) / length()"]]$value, 9)
})

test_that("a pipeline run inside a recorded call leaves the recording to that call", {
  tw <- .runWave()
  p <- pipeline(normalise(), dutyCycle())
  inside <- function(w) .runPipelineWave(p, w)$results[[1]]$value
  res <- tagApply(tw, inside)
  expect_equal(res, dutyCycle(normalise(tw)), ignore_attr = TRUE)
  expect_equal(vapply(attr(res, "processing"), `[[`, character(1), "func"), "inside")
})

test_that("a step that cannot carry on is an error naming the step", {
  tw <- .runWave()
  err <- expect_error(
    .runPipelineWave(pipeline(normalise(), dutyCycle(then = length())), tw),
    class = "pipelineStepError"
  )
  expect_equal(
    conditionMessage(err),
    "Step 2 dutyCycle() gave a numeric rather than a wave, so there is nothing to give its then=."
  )
  expect_equal(err$step, "2")
  expect_error(
    .runPipelineWave(pipeline((function(w) NULL)(then = length())), tw),
    "gave NULL rather than a wave"
  )

  expect_error(
    .runPipelineWave(pipeline(generateTimeShift(amount = c(0.01, 0.02))), tw),
    "^Step 1 generateTimeShift\\(amount = c\\(0.01, 0.02\\)\\) gave a list of 2 waves, and a pipeline carries only one on.$"
  )
  expect_error(
    .runPipelineWave(pipeline(normalise(then = generateTimeShift(amount = 0.01, then = length()))), tw),
    "^Step 1.1 generateTimeShift\\(amount = 0.01\\) gave a list of 1 wave, and then= cannot yet be run on several waves.$"
  )
  # A list that is not of waves is an analysis like any other.
  ran <- .runPipelineWave(pipeline((function(w) list())(), (function(w) list(1))()), tw)
  expect_length(ran$results, 2)
})

test_that("an error in a step's function says which step it was in", {
  tw <- .runWave()
  broken <- function(w) stop("no good")
  err <- expect_error(
    .runPipelineWave(pipeline(normalise(), cutws(1, 100, then = broken())), tw),
    class = "pipelineStepError"
  )
  expect_equal(conditionMessage(err), "Step 2.1 broken() failed: no good")
  expect_equal(err$step, "2.1")
  expect_equal(conditionMessage(err$parent), "no good")
})

test_that("only a pipeline runs, and only on a wave", {
  expect_error(.runPipelineWave(list(), .runWave()), "Can only run a Pipeline, not a list")
  expect_error(.runPipelineWave(pipeline(length()), 1:10), "runs on a Wave or WaveMC object, not an integer")
})

test_that("several waves are told from anything else", {
  w <- .runWave()
  expect_true(.isWaveList(list(w, w)))
  expect_false(.isWaveList(list()))
  expect_false(.isWaveList(list(w, 1)))
  expect_false(.isWaveList(w))
})

test_that("stacked results promote a column to a list, and fill what a table lacks", {
  one <- flattenOutput(list(n = 1, peaks = 440))
  three <- flattenOutput(list(n = 3, peaks = c(440, 880, 1320)))
  other <- flattenOutput(list(n = 0, when = as.Date("2026-11-03")))
  out <- .bindResults(list(one, NULL, three, flattenOutput(NULL), other))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c("value.n", "value.peaks", "value.when"))
  expect_equal(out$value.n, c(1, 3, 0))
  expect_type(out$value.peaks, "list")
  expect_equal(out$value.peaks, list(440, c(440, 880, 1320), NA))
  expect_equal(out$value.when, as.Date(c(NA, NA, "2026-11-03")))

  # A table of several rows keeps them.
  rows <- flattenOutput(data.frame(start = c(0.1, 0.5), end = c(0.2, 0.6)))
  expect_equal(.bindResults(list(rows, rows))$value.start, c(0.1, 0.5, 0.1, 0.5))
  expect_equal(dim(.bindResults(list())), c(0, 0))
})
