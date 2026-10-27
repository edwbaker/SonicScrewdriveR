#A transcript as the Google Speech API returns it: the times are strings holding
#a number of seconds followed by an s.
speechTranscript <- function(starts, ends, words=NULL) {
  if (is.null(words)) {
    words <- paste0("word", seq_along(starts))
  }
  list(timings = data.frame(
    startTime = paste0(starts, "s"),
    endTime = paste0(ends, "s"),
    word = words,
    stringsAsFactors = FALSE
  ))
}

test_that("the seconds suffix is taken off the times", {
  t <- gs_preprocess_transcript(speechTranscript(c(1, 5), c(2, 6)))
  expect_equal(t$timings$startTime, c(1, 5))
  expect_equal(t$timings$endTime, c(2, 6))
  expect_type(t$timings$startTime, "double")
})

test_that("an offset moves every time in the transcript", {
  t <- gs_preprocess_transcript(speechTranscript(c(1, 5), c(2, 6)), offset=10)
  expect_equal(t$timings$startTime, c(11, 15))
  expect_equal(t$timings$endTime, c(12, 16))
})

test_that("the rest of the transcript is left alone", {
  t <- gs_preprocess_transcript(speechTranscript(1, 2, words="hello"))
  expect_equal(t$timings$word, "hello")
})

test_that("something other than a Google transcript is refused", {
  expect_error(labelPadding(list(words="hello")), "Google Speech API")
  expect_error(labelReduction(list(words="hello")), "Google Speech API")
})

test_that("padding moves each label out at both ends", {
  t <- labelPadding(speechTranscript(c(2, 10), c(4, 12)), pad=0.5)
  expect_equal(t$timings$startTime, c(1.5, 9.5))
  expect_equal(t$timings$endTime, c(4.5, 12.5))
})

test_that("padding does not take a label before the start of the recording", {
  t <- labelPadding(speechTranscript(0.2, 1), pad=0.5)
  expect_equal(t$timings$startTime, 0)
})

test_that("padding does not take a label past the end of the recording", {
  t <- labelPadding(speechTranscript(2, 9.8), pad=0.5, max_t=10)
  expect_equal(t$timings$endTime, 10)
  #Without a duration the end is free to run past it.
  expect_equal(labelPadding(speechTranscript(2, 9.8), pad=0.5)$timings$endTime, 10.3)
})

test_that("reduction reports the regions holding speech", {
  res <- labelReduction(speechTranscript(c(1, 5), c(2, 6)))
  expect_named(res, c("starts", "ends"))
  expect_equal(res$starts, c(1, 5))
  expect_equal(res$ends, c(2, 6))
})

test_that("overlapping regions are combined into one", {
  res <- labelReductionExecute(list(starts=c(1, 1.5), ends=c(2, 3)))
  expect_equal(res$starts, 1)
  expect_equal(res$ends, 3)
})

test_that("a region touching the next one is combined with it", {
  res <- labelReductionExecute(list(starts=c(1, 2), ends=c(2, 3)))
  expect_equal(res$starts, 1)
  expect_equal(res$ends, 3)
})

test_that("regions that do not touch are kept apart", {
  res <- labelReductionExecute(list(starts=c(1, 5), ends=c(2, 6)))
  expect_equal(res$starts, c(1, 5))
  expect_equal(res$ends, c(2, 6))
})

test_that("a region wholly inside another is not reported twice", {
  res <- labelReductionExecute(list(starts=c(1, 2), ends=c(10, 3)))
  expect_equal(res$starts, 1)
  expect_equal(res$ends, 10)
})

test_that("regions given out of order are combined", {
  res <- labelReductionExecute(list(starts=c(5, 1), ends=c(6, 2)))
  expect_equal(res$starts, c(1, 5))
  expect_equal(res$ends, c(2, 6))
  joined <- labelReductionExecute(list(starts=c(5, 1), ends=c(6, 5.5)))
  expect_equal(joined$starts, 1)
  expect_equal(joined$ends, 6)
})

test_that("a run of overlapping regions collapses to one", {
  res <- labelReductionExecute(list(starts=c(1, 2, 3, 4), ends=c(2.5, 3.5, 4.5, 5)))
  expect_equal(res$starts, 1)
  expect_equal(res$ends, 5)
})

test_that("no labels gives no regions rather than an error", {
  res <- labelReductionExecute(list(starts=numeric(0), ends=numeric(0)))
  expect_equal(res$starts, numeric(0))
  expect_equal(res$ends, numeric(0))
})

test_that("the regions returned are in order and do not overlap", {
  set.seed(1)
  starts <- runif(50, 0, 100)
  ends <- starts + runif(50, 0.1, 5)
  res <- labelReductionExecute(list(starts=starts, ends=ends))
  expect_true(all(diff(res$starts) > 0))
  expect_true(all(res$ends >= res$starts))
  if (length(res$starts) > 1) {
    expect_true(all(res$starts[-1] > res$ends[-length(res$ends)]))
  }
  #Every input region is inside one of the regions reported.
  covered <- vapply(seq_along(starts), function(i) {
    any(res$starts <= starts[i] & res$ends >= ends[i])
  }, logical(1))
  expect_true(all(covered))
})

test_that("reduction reads the times as numbers, suffix and all", {
  res <- labelReduction(speechTranscript(c(1, 1.5), c(2, 3)))
  expect_equal(res$starts, 1)
  expect_equal(res$ends, 3)
})
