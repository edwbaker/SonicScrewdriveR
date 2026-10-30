test_that("error if insufficient parameters", {
  expect_error(sDuration(), "samp.rate or wave must be specified")
})

test_that("samp.rate paramter works", {
  expect_equal(sDuration(samp.rate=10), 0.1)
  expect_equal(sDuration(n=10, samp.rate=10), 1)
})

test_that("samp.rate from wave functions", {
  expect_silent(sDuration(wave=tuneR::silence(duration=1,samp.rate=10,bit=1)))
  expect_equal(sDuration(wave=tuneR::silence(duration=1,samp.rate=10,bit=1)), 0.1)
})

test_that("tSamples error insufficient parameters", {
  expect_error(tSamples(), "samp.rate or wave must be specified")
})

test_that("tSamples samp.rate parameter works", {
  expect_equal(tSamples(samp.rate=10), 10)
  expect_equal(tSamples(time=10, samp.rate=10), 100)
})

test_that("tSamples from wave functions", {
  expect_silent(tSamples(wave=tuneR::silence(duration=1,samp.rate=10)))
  expect_equal(tSamples(wave=tuneR::silence(duration=1,samp.rate=10)), 10)
})

test_that("sDuration refuses a sample rate at which nothing could be sampled", {
  # sDuration(n=20, samp.rate=0) returned Inf seconds.
  expect_error(sDuration(n=20, samp.rate=0), "Sample rate must be greater than zero.")
  expect_error(sDuration(n=20, samp.rate=-44100), "Sample rate must be greater than zero.")
  expect_error(sDuration(n=20, samp.rate="fast"), "Sample rate must be numeric.")
  expect_equal(sDuration(n=20, samp.rate=44100), 20/44100)
})

test_that("sDuration refuses a negative count of samples", {
  expect_error(sDuration(n=-20, samp.rate=44100), "Number of samples must not be negative.")
  expect_equal(sDuration(n=0, samp.rate=44100), 0)
})

test_that("tSamples refuses an impossible rate or duration", {
  # tSamples(time=-1, samp.rate=44100) returned -44100 samples.
  expect_error(tSamples(time=-1, samp.rate=44100), "Time in Seconds cannot be negative")
  expect_error(tSamples(time=1, samp.rate=0), "Sample rate must be greater than zero.")
  expect_error(tSamples(time=1, samp.rate=-44100), "Sample rate must be greater than zero.")
  expect_equal(tSamples(time=1, samp.rate=44100), 44100)
  expect_equal(tSamples(time=0, samp.rate=44100), 0)
})
