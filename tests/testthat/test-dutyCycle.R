test_that("Correct value is output", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="unit", normalise=FALSE), 0.5)
  d <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_equal(dutyCycle(d, output="unit", normalise=TRUE, limit=0.5), 2/3)
})

test_that("Corect value is output in percantage mode", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(1,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="percent", normalise=FALSE), 50)
})
