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
