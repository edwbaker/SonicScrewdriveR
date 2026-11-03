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
    "^Step 1.1 generateTimeShift\\(amount = 0.01\\) gave a list of 1 wave, and only windowing\\(\\) runs then= on several waves.$"
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

# Five seconds at 8 kHz, without a progress bar for each windowing
.windowWave <- function() {
  tagWave(tuneR::noise("white", duration = 5 * 8000, samp.rate = 8000))
}

test_that("windowing runs its then= on each window, keyed by where the window is", {
  withr::local_options(pboptions = list(type = "none"))
  tw <- .windowWave()
  p <- pipeline(
    normalise(),
    windowing(2, time.unit = "seconds", then = pipeline(
      length(),
      windowing(0.5, time.unit = "seconds", window.overlap = 0.25, then = length())
    )),
    length()
  )
  ran <- .runPipelineWave(p, tw)
  outer <- 'windowing(2, time.unit = "seconds") / length()'
  inner <- paste0('windowing(2, time.unit = "seconds") / ',
                  'windowing(0.5, time.unit = "seconds", window.overlap = 0.25) / length()')
  expect_named(ran$results, c(outer, inner, "length()"))

  # The two whole windows of two seconds, the half second left over not filling
  # a third.
  expect_equal(ran$results[[outer]], data.frame(
    window_start = c(0, 2), window_end = c(2, 4), window_length = c(2, 2),
    value = c(16000L, 16000L)
  ))
  # A windowing inside another numbers its columns, and counts from the start
  # of the wave the pipeline was given rather than from its window.
  rows <- ran$results[[inner]]
  expect_named(rows, c("window_start", "window_end", "window_length",
                       "window_start_2", "window_end_2", "window_length_2", "value"))
  expect_equal(nrow(rows), 14)
  expect_equal(rows$window_start_2[rows$window_start == 2],
               seq(2, 3.5, by = 0.25))
  expect_equal(unique(rows$window_length_2), 0.5)
  expect_equal(unique(rows$value), 4000L)

  # The steps after the windowing get the wave as it was, which keeps no record
  # of the windows.
  expect_equal(ran$results[["length()"]]$value, 40000L)
  expect_equal(.historyOf(ran$wave), c("normalise", "length"))
})

test_that("a window's history is the wave's, then the cut that made it", {
  withr::local_options(pboptions = list(type = "none"))
  history <- function(w) paste(vapply(w@processing, `[[`, character(1), "func"), collapse = " ")
  cut <- function(w) Filter(function(r) identical(r$func, "cutws"), w@processing)[[1]]$params
  ran <- .runPipelineWave(
    pipeline(normalise(), windowing(16000, then = pipeline(history(), cut()))),
    .windowWave()
  )
  expect_equal(ran$results[[1]]$value, c("normalise cutws", "normalise cutws"))
  expect_equal(ran$results[[2]]$value.from, c(1, 16001))
  expect_equal(ran$results[[2]]$value.to, c(16000, 32000))
})

test_that("a last window shorter than the rest is keyed by the audio it holds", {
  withr::local_options(pboptions = list(type = "none"))
  ran <- .runPipelineWave(
    pipeline(windowing(2, time.unit = "seconds", complete.windows = FALSE, then = length())),
    .windowWave()
  )
  rows <- ran$results[[1]]
  expect_equal(rows$window_start, c(0, 2, 4))
  expect_equal(rows$window_end, c(2, 4, 5))
  expect_equal(rows$window_length, c(2, 2, 1))
})

test_that("results from windows stack as results from files will", {
  withr::local_options(pboptions = list(type = "none"))
  # One window finds one peak and the next three, so the column is a list.
  peaks <- function(w) if (w@processing[[length(w@processing)]]$params$from == 1) 440 else c(1, 2, 3)
  nothing <- function(w) NULL
  ran <- .runPipelineWave(
    pipeline(windowing(16000, then = pipeline(peaks(), nothing()))),
    .windowWave()
  )
  expect_type(ran$results[["windowing(16000) / peaks()"]]$value, "list")
  expect_equal(ran$results[["windowing(16000) / peaks()"]]$value, list(440, c(1, 2, 3)))
  # A step that found nothing in any window keeps the key columns and no rows.
  empty <- ran$results[["windowing(16000) / nothing()"]]
  expect_equal(nrow(empty), 0)
  expect_named(empty, c("window_start", "window_end", "window_length"))
})

test_that("a cut is placed in the recording, with or without a windowing", {
  withr::local_options(pboptions = list(type = "none"))
  tw <- .windowWave()
  ran <- .runPipelineWave(pipeline(
    # Through then=, in samples and in seconds.
    cutws(16001, 24000, then = length()),
    seewave::cutw(from = 3, to = 4, output = "Wave", then = length()),
    # As a step of its own, cutting the wave the steps after it get.
    cutws(8001, 40000),
    length(),
    # A cut of a cut gives the span measured, from the start of the recording.
    cutws(8001, 16000, then = length())
  ), tw)
  rows <- unname(ran$results)
  expect_equal(rows[[1]], data.frame(cut_start = 2, cut_end = 3, value = 8000L))
  expect_equal(rows[[2]], data.frame(cut_start = 3, cut_end = 4, value = 8000L))
  expect_equal(rows[[3]], data.frame(cut_start = 1, cut_end = 5, value = 32000L))
  expect_equal(rows[[4]], data.frame(cut_start = 2, cut_end = 3, value = 8000L))

  # A cut with no start begins at the start, and one past the end ends there.
  ran <- .runPipelineWave(pipeline(
    seewave::cutw(to = 1, output = "Wave", then = length()),
    cutws(32001, then = length())
  ), tw)
  expect_equal(ran$results[[1]][c("cut_start", "cut_end")], data.frame(cut_start = 0, cut_end = 1))
  expect_equal(ran$results[[2]][c("cut_start", "cut_end")], data.frame(cut_start = 4, cut_end = 5))

  # A wave without tags is placed the same way, from the step's arguments.
  ran <- .runPipelineWave(pipeline(cutws(16001, 24000, then = length())), untagWave(tw))
  expect_equal(ran$results[[1]]$cut_start, 2)
})

test_that("windows inside a cut are counted from the start of the recording", {
  withr::local_options(pboptions = list(type = "none"))
  tw <- .windowWave()
  ran <- .runPipelineWave(pipeline(
    task(cutws, 16001, 32000, then = windowing(1, time.unit = "seconds", then = length())),
    windowing(16000, then = seewave::cutw(from = 0.5, to = 1, output = "Wave", then = length()))
  ), tw)
  inside_cut <- ran$results[[1]]
  expect_named(inside_cut, c("cut_start", "cut_end", "window_start", "window_end",
                             "window_length", "value"))
  expect_equal(unique(inside_cut$cut_start), 2)
  expect_equal(inside_cut$window_start, c(2, 3))
  # A cut inside a window is placed from the start of the recording too.
  cut_in_window <- ran$results[[2]]
  expect_named(cut_in_window, c("window_start", "window_end", "window_length",
                                "cut_start", "cut_end", "value"))
  expect_equal(cut_in_window$cut_start, c(0.5, 2.5))
  expect_equal(cut_in_window$cut_end, c(1, 3))
})

test_that("only cutws() and seewave's cutw() are taken for cuts", {
  withr::local_options(pboptions = list(type = "none"))
  # A function of the user's that cuts is not recognised, and the windows after
  # it are placed as though it had not cut.
  trim <- function(w) cutws(w, 16001, 32000)
  ran <- .runPipelineWave(pipeline(trim(then = windowing(8000, then = length()))), .windowWave())
  expect_named(ran$results[[1]], c("window_start", "window_end", "window_length", "value"))
  expect_equal(ran$results[[1]]$window_start, c(0, 1))
  # Nor is cutw() giving its default matrix, which is an analysis.
  ran <- .runPipelineWave(pipeline(seewave::cutw(from = 0, to = 1)), .windowWave())
  expect_false("cut_start" %in% names(ran$results[[1]]))
})

test_that("a windowing that cannot run says which step it is", {
  withr::local_options(pboptions = list(type = "none"))
  short <- tagWave(tuneR::noise("white", duration = 8000, samp.rate = 8000))
  err <- expect_error(
    .runPipelineWave(pipeline(windowing(2, time.unit = "seconds", then = length())), short),
    class = "pipelineStepError"
  )
  expect_equal(
    conditionMessage(err),
    paste0('Step 1 windowing(2, time.unit = "seconds") gave no windows, as the wave, ',
           "of 1 second, is shorter than one window.")
  )
  expect_error(
    .runPipelineWave(pipeline(windowing(4, time.unit = "seconds", then = length())),
                     tagWave(tuneR::noise("white", duration = 24000, samp.rate = 8000))),
    "of 3 seconds, is shorter than one window"
  )

  # An error from a step inside a window names that step, once.
  broken <- function(w) stop("no good")
  err <- expect_error(
    .runPipelineWave(pipeline(windowing(16000, then = broken())), .windowWave()),
    class = "pipelineStepError"
  )
  expect_equal(conditionMessage(err), "Step 1.1 broken() failed: no good")
  # And one from windowing itself names the windowing.
  expect_error(
    .runPipelineWave(pipeline(windowing(16000, window.overlap = 16000, then = length())),
                     .windowWave()),
    "^Step 1 windowing\\(16000, window.overlap = 16000\\) failed: In windowing window.overlap"
  )
})
