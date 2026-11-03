# What a process gives back is flattened into rows, so that the results of one
# process over many recordings, or many windows of one, bind into one table.

test_that("nothing gives no rows", {
  flat <- flattenOutput(NULL)
  expect_s3_class(flat, "data.frame")
  expect_equal(dim(flat), c(0, 0))
})

test_that("a single value is one row with one column", {
  expect_equal(flattenOutput(0.42), data.frame(value=0.42))
  expect_equal(flattenOutput("song"), data.frame(value="song"))
  # A value with a class keeps it.
  day <- as.Date("2026-11-03")
  expect_equal(flattenOutput(day)$value, day)
})

test_that("a named vector gives a column for each element", {
  expect_equal(
    flattenOutput(c(mean=440, sd=7.5)),
    data.frame(value.mean=440, value.sd=7.5)
  )
  # A name is kept however many elements there are, so the columns depend on
  # the names alone.
  expect_equal(flattenOutput(c(peak=440)), data.frame(value.peak=440))
  # A class is kept on each element.
  levels <- factor(c(a="x", b="y"))
  expect_equal(flattenOutput(levels)$value.b, factor("y", levels=c("x", "y")))
})

test_that("a vector without names is kept whole in one cell", {
  flat <- flattenOutput(c(440, 880, 1320))
  expect_equal(dim(flat), c(1, 1))
  expect_named(flat, "value")
  expect_equal(flat$value[[1]], c(440, 880, 1320))
  # Including one that is empty, which found nothing rather than gave nothing.
  expect_equal(flattenOutput(numeric(0))$value[[1]], numeric(0))
  # Names that cannot each make a column leave the vector whole, names and all.
  expect_equal(flattenOutput(c(a=1, 2))$value[[1]], c(a=1, 2))
  expect_equal(flattenOutput(c(a=1, a=2))$value[[1]], c(a=1, a=2))
})

test_that("a data frame keeps its rows", {
  # A detector's events, or an index measured over each window, are rows of
  # their own and stay so.
  events <- data.frame(start=c(0.1, 0.6), end=c(0.3, 0.9))
  expect_equal(
    flattenOutput(events),
    data.frame(value.start=c(0.1, 0.6), value.end=c(0.3, 0.9))
  )
  # A detector that found nothing gives no rows, not a row of nothing.
  none <- flattenOutput(events[0, ])
  expect_equal(nrow(none), 0)
  expect_named(none, c("value.start", "value.end"))
  # A data frame with no columns has nothing to name.
  expect_equal(dim(flattenOutput(data.frame())), c(0, 0))
})

test_that("row names a data frame was given are kept, numbers are not", {
  phases <- data.frame(start=c(1, 2), row.names=c("dawn", "dusk"))
  expect_equal(
    flattenOutput(phases),
    data.frame(value.rownames=c("dawn", "dusk"), value.start=c(1, 2))
  )
  # What is left of the numbers after taking some rows says nothing.
  some <- data.frame(start=c(1, 2, 3))[2:3, , drop=FALSE]
  expect_named(flattenOutput(some), "value.start")
  expect_equal(.row_names_info(flattenOutput(some)), -2L)
})

test_that("a named list is flattened all the way down", {
  out <- list(
    n=2,
    spread=c(low=1, high=2),
    first=data.frame(start=0.1, label="call"),
    nothing=NULL
  )
  expect_equal(
    flattenOutput(out),
    data.frame(
      value.n=2, value.spread.low=1, value.spread.high=2,
      value.first.start=0.1, value.first.label="call"
    )
  )
  # As seewave::specprop() returns, a list of single values and one that
  # carries a stray name.
  props <- list(mean=440, sd=7.5, prec=c(x=10))
  expect_named(flattenOutput(props), c("value.mean", "value.sd", "value.prec.x"))
})

test_that("a data frame within a list keeps its columns whatever its length", {
  # One row is a value in each column; more than one is the whole column in a
  # cell. The columns are the same either way, so rows from different windows
  # line up.
  out <- list(n=3, events=data.frame(start=c(0.1, 0.5, 0.9), label=c("a", "b", "c")))
  flat <- flattenOutput(out)
  expect_equal(dim(flat), c(1, 3))
  expect_named(flat, c("value.n", "value.events.start", "value.events.label"))
  expect_equal(flat$value.events.start[[1]], c(0.1, 0.5, 0.9))
  expect_equal(flat$value.events.label[[1]], c("a", "b", "c"))

  # A list column holds its one value in a list, which is what is flattened.
  one <- data.frame(start=0.1)
  one$peaks <- list(c(440, 880))
  flat <- flattenOutput(list(event=one))
  expect_named(flat, c("value.event.start", "value.event.peaks"))
  expect_equal(flat$value.event.peaks[[1]], c(440, 880))
})

test_that("anything else is kept whole in one cell", {
  spectrum <- matrix(1:4, ncol=2)
  expect_equal(flattenOutput(spectrum)$value[[1]], spectrum)

  # A list with a class of its own is an object rather than a set of results,
  # however it is built.
  test <- stats::t.test(1:10)
  flat <- flattenOutput(test)
  expect_equal(dim(flat), c(1, 1))
  expect_equal(flat$value[[1]], test)

  # As is a list some of whose elements have no name to give a column.
  expect_equal(flattenOutput(list(a=1, 2))$value[[1]], list(a=1, 2))
  expect_equal(flattenOutput(list())$value[[1]], list())
})

test_that("every column begins with value and no name is given twice", {
  # So that a table of results can name its own columns - the recording, the
  # window, the process - anything else.
  out <- list(a.b=1, a=list(b=2), file="x")
  flat <- flattenOutput(out)
  expect_named(flat, c("value.a.b", "value.a.b.1", "value.file"))
  expect_true(all(startsWith(names(flat), "value")))
})

test_that("the history tagApply attaches to a result is not part of it", {
  tw <- tagWave(tuneR::sine(440, duration=4410, samp.rate=44100))
  level <- tagApply(tw, function(x) mean(abs(x@left)))
  expect_false(is.null(attr(level, "processing")))
  flat <- flattenOutput(level)
  expect_equal(flat, data.frame(value=mean(abs(tw@left))))
  expect_null(attr(flat$value, "processing"))

  events <- tagApply(tw, function(x) data.frame(start=0, end=0.1))
  expect_null(attr(flattenOutput(events), "processing"))

  # An environment is not copied when it is changed, so the caller's would
  # lose the attribute too: it is kept whole, as it is.
  model <- new.env()
  attr(model, "processing") <- list("history")
  flat <- flattenOutput(model)
  expect_identical(flat$value[[1]], model)
  expect_equal(attr(model, "processing"), list("history"))
})

test_that("flattened rows from different recordings bind", {
  rows <- do.call(rbind, lapply(1:3, function(i) {
    flattenOutput(data.frame(start=seq_len(i)))
  }))
  expect_equal(nrow(rows), 6)
  expect_equal(rows$value.start, c(1, 1, 2, 1, 2, 3))
})
