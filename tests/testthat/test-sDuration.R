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
