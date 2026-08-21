test_that("inputs are correct", {
  expect_error(frequencyStats("string"), "Expecting a Wave object")
  expect_error(frequencyStats(1), "Expecting a Wave object")
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE))
})

test_that("frequencyStats analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)

  expect_false(isTRUE(all.equal(frequencyStats(L), frequencyStats(R))))

  # frequencyStats returns a list, so allChannels() does not wrap it again.
  expect_equal(frequencyStats(tuneR::stereo(L, R)), list(frequencyStats(L), frequencyStats(R)))
  expect_equal(
    frequencyStats(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)),
    list(frequencyStats(L), frequencyStats(R))
  )
})

test_that("frequencyStats passes arguments to each channel", {
  sr <- 8000
  n <- 8000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, L)

  expect_equal(frequencyStats(st, lowcut=2), list(frequencyStats(L, lowcut=2), frequencyStats(L, lowcut=2)))
  expect_equal(frequencyStats(st, warn=FALSE), list(frequencyStats(L, warn=FALSE), frequencyStats(L, warn=FALSE)))
})
