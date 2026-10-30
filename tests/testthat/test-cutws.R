test_that("inputs are correct", {
  expect_error(cutws("string", from=1, to=2), "Expecting a Wave or WaveMC object")
  expect_error(cutws(1, from=1, to=2), "Expecting a Wave or WaveMC object")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1, to ="2"), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = "1", to =2), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1, to =2.5), "In cutws both from and to must be integers")
  expect_error(cutws(tuneR::sine(1000, duration=10), from = 1.5, to =2), "In cutws both from and to must be integers")
  expect_silent(cutws(tuneR::sine(1000, duration=10), from = 1, to =2))
})

test_that("to must be greater than from", {
  expect_error(cutws(tuneR::sine(1000, duration=10), from=20, to = 1), "In cutws to must be greater than from")
})

test_that("plotting is ok in cutws", {
  expect_silent(cutws(tuneR::sine(1000, duration=10), from = 1, to =2, plot=TRUE))
})

test_that("cutws returns a Wave object", {
  expect_true(inherits(cutws(tuneR::sine(1000, duration=10), from = 1, to =2), "Wave"))
})

test_that("Handles Inf correctly", {
  w <- tuneR::sine(1000, duration=44100, samp.rate = 44100)
  expect_equal(length(cutws(w, from = 44001, to = Inf)@left), 100)
})

test_that("cutws returns a WaveMC object for WaveMC input", {
  w <- tuneR::sine(1000, duration=100, samp.rate=8000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left, w@left), samp.rate=8000, bit=16)
  colnames(mc@.Data) <- c("A", "B", "C")

  cut <- cutws(mc, from=51, to=60)
  expect_true(inherits(cut, "WaveMC"))
  expect_equal(nrow(cut@.Data), 10)
  expect_equal(ncol(cut@.Data), 3)
  expect_equal(cut@.Data[,1], mc@.Data[51:60,1])
})

test_that("cutws preserves WaveMC properties", {
  w <- tuneR::sine(1000, duration=100, samp.rate=8000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate=8000, bit=16)
  colnames(mc@.Data) <- c("left", "right")

  cut <- cutws(mc, from=1, to=10)
  expect_equal(colnames(cut@.Data), c("left", "right"))
  expect_equal(cut@samp.rate, mc@samp.rate)
  expect_equal(cut@bit, mc@bit)
  expect_equal(cut@pcm, mc@pcm)
})

test_that("cutws handles Inf and plotting for WaveMC", {
  w <- tuneR::sine(1000, duration=100, samp.rate=8000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate=8000, bit=16)

  expect_equal(nrow(cutws(mc, from=91, to=Inf)@.Data), 10)
  expect_silent(cutws(mc, from=1, to=10, plot=TRUE))
})

test_that("cutws preserves pcm for Wave objects", {
  s <- tuneR::sine(440, duration=1000, samp.rate=8000)
  expect_false(s@pcm)
  cut <- cutws(s, from=1, to=500)
  expect_false(cut@pcm)
  expect_equal(cut@bit, s@bit)

  stereo <- tuneR::Wave(s@left, right=s@left, samp.rate=8000, bit=32, pcm=FALSE)
  expect_false(cutws(stereo, from=1, to=500)@pcm)
})

test_that("cutws can cut a 64-bit float Wave", {
  w64 <- tuneR::Wave(stats::runif(1000, -1, 1), samp.rate=8000, bit=64, pcm=FALSE)
  cut <- cutws(w64, from=1, to=500)
  expect_equal(length(cut@left), 500)
  expect_equal(cut@bit, 64)
  expect_false(cut@pcm)
})

test_that("cutws past the end returns the audio that is there, not NA", {
  w <- tuneR::sine(1000, duration=100, samp.rate=8000)
  cut <- cutws(w, from=91, to=110)
  expect_equal(length(cut@left), 10)
  expect_equal(sum(is.na(cut@left)), 0)
  expect_equal(cut@left, w@left[91:100])

  mc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate=8000, bit=16)
  cutmc <- cutws(mc, from=91, to=110)
  expect_equal(nrow(cutmc@.Data), 10)
  expect_equal(sum(is.na(cutmc@.Data)), 0)

  expect_error(cutws(w, from=110, to=120), "In cutws to must be greater than from")
})

test_that("from is clamped to the first sample as to is clamped to the last", {
  w <- tuneR::Wave(as.numeric(1:10), samp.rate=8000, bit=32, pcm=FALSE)

  # A zero subscript is dropped by R rather than refused, so from=0 used to give
  # a section one sample short and shifted by one, with no error at all.
  expect_equal(cutws(w, from=0, to=3)@left, as.numeric(1:3))
  expect_equal(cutws(w, from=-5, to=3)@left, as.numeric(1:3))

  # Clamping below matches the documented clamping of to above the end.
  expect_equal(cutws(w, from=0, to=Inf)@left, as.numeric(1:10))
  expect_equal(cutws(w, from=1, to=3)@left, as.numeric(1:3))
})

test_that("a from or to that is not one whole number is refused by name", {
  w <- tuneR::Wave(as.numeric(1:10), samp.rate=8000, bit=32, pcm=FALSE)

  # as.integer() is NA past the largest integer, so these aborted with R's
  # "missing value where TRUE/FALSE needed" rather than naming from and to.
  # 3e9 is a whole number, so it is not refused for that: it is past the end of
  # the wave, and now says so.
  expect_error(cutws(w, from=3e9, to=10), "In cutws to must be greater than from")
  expect_error(cutws(w, from=NA, to=10), "In cutws both from and to must be integers")
  expect_error(cutws(w, from=1, to=NA), "In cutws both from and to must be integers")
  expect_error(cutws(w, from=c(1, 2), to=10), "In cutws both from and to must be integers")
})
