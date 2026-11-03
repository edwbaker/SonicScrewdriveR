.pipeWave <- function() {
  tagWave(noise("white", duration=4410, samp.rate=44100))
}

# The calls of a pipeline's steps, as they are shown
.pipeCalls <- function(p) {
  vapply(p$steps, function(s) .deparseOne(s$call), character(1))
}

test_that("pipeline() keeps each call as written and runs none of them", {
  ran <- 0
  counted <- function(wave, n=1) {
    ran <<- ran + 1
    n
  }
  p <- pipeline(normalise(), counted(n = 2), filterWave(bandpass(2000, 11000)))
  expect_s3_class(p, "Pipeline")
  expect_named(p, "steps")
  expect_equal(ran, 0)
  expect_equal(
    .pipeCalls(p),
    c("normalise()", "counted(n = 2)", "filterWave(bandpass(2000, 11000))")
  )
})

test_that("the arguments of a step are evaluated once, when the pipeline is made", {
  made <- 0
  low <- 2000
  filt <- function() {
    made <<- made + 1
    bandpass(low, 11000)
  }
  p <- pipeline(filterWave(filt()), dutyCycle(limit = low / 10000))
  expect_equal(made, 1)
  low <- 500
  expect_s4_class(p$steps[[1]]$args[[1]], "WaveFilter")
  expect_equal(p$steps[[1]]$args[[1]]@params$from, 2000)
  expect_equal(p$steps[[2]]$args, list(limit = 0.2))
  expect_equal(.pipeCalls(p)[2], "dutyCycle(limit = low/10000)")

  # Arguments forwarded from the function the pipeline is written in are
  # evaluated there, and a function of the user's called list() is not used to
  # gather them.
  make <- function(...) {
    list <- function(...) stop("not this one")
    pipeline(dutyCycle(...))
  }
  expect_equal(make(limit = 0.3)$steps[[1]]$args, list(limit = 0.3))
})

test_that("then= is taken out of the call and becomes the step's own pipeline", {
  p <- pipeline(filterWave(bandpass(2000, 11000), then = pipeline(
    dutyCycle(),
    cutws(1, 100, then = dutyCycle())
  )))
  s <- p$steps[[1]]
  expect_equal(.deparseOne(s$call), "filterWave(bandpass(2000, 11000))")
  expect_false("then" %in% names(s$args))
  expect_s3_class(s$then, "Pipeline")
  expect_length(s$then$steps, 2)
  expect_s3_class(s$then$steps[[2]]$then, "Pipeline")

  # A single step is a pipeline of one, and NULL is no then= at all.
  one <- pipeline(normalise(then = dutyCycle()))$steps[[1]]$then
  expect_equal(.pipeCalls(one), "dutyCycle()")
  expect_null(pipeline(normalise(then = NULL))$steps[[1]]$then)

  expect_error(
    pipeline(normalise(then = dutyCycle(), then = dutyCycle())),
    "then= is given more than once"
  )
})

test_that("pipelines join, and a pipeline is a step", {
  p1 <- pipeline(normalise(), dutyCycle())
  p2 <- pipeline(beatSpectrum())
  joined <- pipeline(p1, p2)
  expect_equal(.pipeCalls(joined), c("normalise()", "dutyCycle()", "beatSpectrum()"))

  # Given by name, in a list, within operators that only reach a value, and as
  # the then= of another step.
  ps <- list(p1, p2)
  expect_equal(.pipeCalls(pipeline(ps)), .pipeCalls(joined))
  expect_equal(.pipeCalls(pipeline(ps[[2]])), "beatSpectrum()")
  expect_equal(.pipeCalls(pipeline((p2), {p2}, if (TRUE) p2)), rep("beatSpectrum()", 3))
  expect_equal(.pipeCalls(do.call(pipeline, ps)), .pipeCalls(joined))
  expect_equal(.pipeCalls(pipeline(sonicscrewdriver::pipeline(dutyCycle()))), "dutyCycle()")
  nested <- pipeline(filterWave(bandpass(1, 2), then = p1))
  expect_equal(.pipeCalls(nested$steps[[1]]$then), .pipeCalls(p1))

  # Nothing, and an empty list, are no steps.
  expect_length(pipeline()$steps, 0)
  expect_length(pipeline(NULL, list())$steps, 0)

  # The functions of base R that build a list are evaluated rather than taken
  # for steps, so lapply() can be written in place.
  built <- pipeline(lapply(c(1, 60), function(n) task(dutyCycle, n)))
  expect_equal(.pipeCalls(built), c("dutyCycle(1)", "dutyCycle(60)"))
  expect_equal(.pipeCalls(pipeline(c(list(p1), list(p2)), rev(list(p2)))),
               c(.pipeCalls(joined), "beatSpectrum()"))
  expect_error(pipeline(c(p1, p2)), "is a list")
})

test_that("a windowing in a pipeline takes then= as its FUN", {
  p <- pipeline(windowing(60, time.unit = "seconds", then = pipeline(dutyCycle(), length())))
  expect_equal(.pipeCalls(p), 'windowing(60, time.unit = "seconds")')
  expect_equal(.pipeCalls(p$steps[[1]]$then), c("dutyCycle()", "length()"))
  expect_equal(.pipeCalls(task(windowing, 60, time.unit = "seconds", then = dutyCycle())),
               'windowing(60, time.unit = "seconds")')
  # Given a FUN of its own and no then=, it is an analysis like any other.
  expect_silent(pipeline(windowing(60, FUN = function(w, ...) 1)))

  expect_error(pipeline(windowing(60, FUN = length, then = dutyCycle())),
               "windowing\\(\\) is given both FUN and then=")
  expect_error(pipeline(windowing(60, length, then = dutyCycle())),
               "windowing\\(\\) is given both FUN and then=")
  expect_error(pipeline(windowing(60)), "windowing\\(\\) in a pipeline needs then=")
  expect_error(task(windowing, 60), "windowing\\(\\) in a pipeline needs then=")

  # time.unit comes after ..., so a misspelt or shortened one would be handed to
  # FUN, which then= leaves nothing to take, and the windows would be measured
  # in samples. It is refused instead.
  expect_error(pipeline(windowing(60, time.units = "seconds", then = dutyCycle())),
               "windowing\\(\\) is given time.units, which is none of its arguments")
  expect_error(pipeline(windowing(60, time.u = "seconds", tu = 1, then = dutyCycle())),
               "windowing\\(\\) is given time.u, tu, which are none of its arguments")
  # Without then=, they are FUN's, as they always were.
  expect_silent(pipeline(windowing(60, FUN = function(w, s, l, k) k, k = 1)))
})

test_that("anything else given to pipeline() is an error that says what it was", {
  expect_error(pipeline(normalise), "`normalise` is a function.*`normalise\\(\\)`")
  expect_error(pipeline(dutyCycle(), ), "Step 2 of pipeline\\(\\) is empty")
  expect_error(pipeline(function(w) 1), "is a function")
  expect_error(pipeline(5), "`5` is a numeric")
  expect_error(pipeline(list(1, 2)), "is a list")
  # A pipeline is given its recordings when it runs, so that one pipeline can
  # run on any number of sets.
  set <- recordingSet(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_error(pipeline(dutyCycle(), recordings = set),
               "`set` is a RecordingSet. A pipeline does not carry the recordings it is run on")
  expect_error(pipeline(set), "is a RecordingSet")
  expect_error(pipeline(noSuchFunction()), "Could not find the function of the step noSuchFunction")
  holder <- list(f = 1)
  expect_error(pipeline(holder$f()), "Could not find the function of the step holder\\$f")
})

test_that("a step's function is found as R finds the function of a call", {
  # A variable of the same name that is not a function is passed over.
  dutyCycle <- 5
  p <- pipeline(dutyCycle(), "normalise"(), (function(w) 1)())
  expect_identical(p$steps[[1]]$fun, sonicscrewdriver::dutyCycle)
  expect_identical(p$steps[[2]]$fun, sonicscrewdriver::normalise)
  expect_true(is.function(p$steps[[3]]$fun))
  expect_equal(.pipeCalls(p)[3], "(function(w) 1)()")
})

test_that("task() makes the same step as a call", {
  low <- 2000
  written <- pipeline(filterWave(bandpass(2000, 11000), then = dutyCycle()))
  built <- task(filterWave, bandpass(low, 11000), then = dutyCycle())
  expect_s3_class(built, "Pipeline")
  expect_length(built$steps, 1)
  expect_identical(built$steps[[1]]$fun, written$steps[[1]]$fun)
  expect_equal(built$steps[[1]]$args, written$steps[[1]]$args)
  expect_equal(.pipeCalls(built$steps[[1]]$then), "dutyCycle()")
  expect_equal(.pipelineId(built), .pipelineId(written))

  # then= takes what pipeline() takes, and NULL or nothing is none.
  expect_equal(.pipeCalls(task(normalise, then = pipeline(dutyCycle()))$steps[[1]]$then),
               "dutyCycle()")
  expect_null(task(normalise, then = NULL)$steps[[1]]$then)
  expect_null(task(normalise)$steps[[1]]$then)

  # The function can be named, and must be one.
  expect_identical(task("normalise")$steps[[1]]$fun, sonicscrewdriver::normalise)
  expect_error(task(5), "task\\(\\) needs a function, or the name of one, not 5")
  expect_error(task("noSuchFunction"), "not \"noSuchFunction\"")

  # An argument called f reaches the function rather than task().
  expect_equal(task(function(w, f) f, f = 3)$steps[[1]]$args, list(f = 3))
})

test_that("task() inside lapply() shows each step as it will run", {
  bands <- lapply(c(1000, 4000), function(f) {
    task(filterWave, bandpass(f, 2 * f), then = dutyCycle())
  })
  p <- do.call(pipeline, bands)
  expect_equal(.pipeCalls(p), c(
    "filterWave(bandpass(1000, 2 * 1000))", "filterWave(bandpass(4000, 2 * 4000))"
  ))
  expect_equal(p$steps[[2]]$args[[1]]@params, list(from=4000, to=8000))

  # A short value is shown as itself, a long one as written, and a value
  # given with nothing written by its class.
  lengths <- lapply(c(1, 60), function(n) task(dutyCycle, n))
  expect_equal(.pipeCalls(do.call(pipeline, lengths)), c("dutyCycle(1)", "dutyCycle(60)"))
  long <- seq_len(20)
  expect_equal(.pipeCalls(task(dutyCycle, long)), "dutyCycle(long)")
  # A name that means the same thing anywhere is left as it is.
  expect_equal(.pipeCalls(task(dutyCycle, rep(pi, 20))), "dutyCycle(rep(pi, 20))")
  expect_equal(.pipeCalls(do.call(task, list(filterWave, bandpass(1, 2)))),
               "sonicscrewdriver::filterWave(<WaveFilter>)")
  expect_equal(.pipeCalls(do.call(task, list(dutyCycle, limit = NULL))),
               "sonicscrewdriver::dutyCycle(limit = NULL)")

  # Only a variable of the function is shown as its value: not the name of a
  # part after $ or @, not a value too long to show, and not an empty argument.
  shown <- local({
    limit <- 0.5
    long <- seq_len(20)
    nothing <- NULL
    settings <- list(limit = 0.2)
    m <- matrix(1:4, 2)
    task(dutyCycle, list(settings$limit, long, nothing, m[, 1], limit))
  })
  expect_equal(.pipeCalls(shown),
               "dutyCycle(list(settings$limit, long, NULL, m[, 1], 0.5))")
})

test_that("task() shows a function by a name that means something outside it", {
  fs <- list(normalise, function(w) 1)
  steps <- lapply(fs, function(f) task(f))
  expect_equal(.pipeCalls(steps[[1]]), "sonicscrewdriver::normalise()")
  expect_equal(.pipeCalls(steps[[2]]), "(function (w) 1)()")
  expect_equal(.pipeCalls(task(normalise)), "normalise()")
  expect_equal(.pipeCalls(task(sonicscrewdriver::normalise)), "sonicscrewdriver::normalise()")
  expect_equal(.pipeCalls(task(sonicscrewdriver:::.normalise)),
               "sonicscrewdriver:::.normalise()")
  expect_equal(.pipeCalls(task(function(w) 2)), "(function(w) 2)()")
  expect_equal(.pipeCalls(task(length)), "length()")

  # A function of the user's is shown by its name where that is a name of the
  # global environment, and by its code where it is a variable of a function.
  withr::defer(rm("pipelineTestStep", envir = globalenv()))
  assign("pipelineTestStep", function(w) 3, envir = globalenv())
  expect_equal(.pipeCalls(eval(quote(task(pipelineTestStep)), globalenv())),
               "pipelineTestStep()")
  mine <- function(w) 3
  expect_equal(.pipeCalls(task(mine)), "(function (w) 3)()")

  # A name looked for where nothing leads to a package or the global
  # environment is not taken for a variable.
  expect_null(.localFrame("x", new.env(parent = emptyenv())))
})

test_that("printing a pipeline gives its outline, numbered, and the code it is identified by", {
  p <- pipeline(
    normalise(),
    dutyCycle(),
    filterWave(bandpass(2000, 11000), then = pipeline(
      dutyCycle(),
      cutws(1, 1000, then = dutyCycle())
    )),
    cutws(1, 2000, then = filterWave(bandpass(2000, 11000), then = dutyCycle()))
  )
  out <- format(p)
  expect_match(out[1], "^<Pipeline [0-9a-f]{8}: 9 steps>$")
  expect_equal(out[2:10], c(
    "1. normalise()",
    "2. dutyCycle()",
    "3. filterWave(bandpass(2000, 11000)), then",
    "   3.1 dutyCycle()",
    "   3.2 cutws(1, 1000), then",
    "       3.2.1 dutyCycle()",
    "4. cutws(1, 2000), then",
    "   4.1 filterWave(bandpass(2000, 11000)), then",
    "       4.1.1 dutyCycle()"
  ))
  # covr writes calls to itself into this package's functions while it measures
  # coverage, so a coverage run counts it too.
  expect_match(
    out[11],
    "^Code: sonicscrewdriver [0-9a-f]{8}, (covr [0-9a-f]{8}, )?digest [0-9a-f]{8}, seewave [0-9a-f]{8}, tuneR [0-9a-f]{8}, R .+$"
  )
  expect_equal(substr(out[1], 11, 18), substr(.pipelineId(p), 1, 8))
  expect_output(expect_invisible(print(p)), "4.1.1 dutyCycle()", fixed = TRUE)

  expect_match(format(task(normalise))[1], ": 1 step>$")
  empty <- format(pipeline())
  expect_length(empty, 2)
  expect_match(empty[1], "^<Pipeline [0-9a-f]{8}: 0 steps>$")
  expect_equal(empty[2], "Code: none")
})

test_that("a pipeline that cannot be identified still prints its outline", {
  p <- pipeline(dutyCycle(limit = new.env()))
  out <- format(p)
  expect_equal(out[1], "<Pipeline: 1 step>")
  expect_equal(out[2], "1. dutyCycle(limit = new.env())")
  expect_match(out[3], "^Code: not hashed, as A pipeline cannot be identified by an argument holding an environment")
})

test_that("code that is not a hash is shown whole", {
  expect_equal(
    .shortCode(c(sonicscrewdriver = strrep("a", 64), devtools = "not installed",
                 R = "4.6.1 (2026-06-24 ucrt)")),
    c("aaaaaaaa", "not installed", "4.6.1 (2026-06-24 ucrt)")
  )
})

test_that("a description of a value has the article it needs", {
  expect_equal(.aDescription(1), "a numeric")
  expect_equal(.aDescription(new.env()), "an environment")
})
