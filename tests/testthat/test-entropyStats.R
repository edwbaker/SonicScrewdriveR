test_that("entropyStats analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)

  # The channels must give different answers for the test to mean anything.
  expect_false(isTRUE(all.equal(entropyStats(L), entropyStats(R))))

  # entropyStats returns a list, so allChannels() does not wrap it again.
  expect_equal(entropyStats(tuneR::stereo(L, R)), list(entropyStats(L), entropyStats(R)))
  expect_equal(
    entropyStats(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)),
    list(entropyStats(L), entropyStats(R))
  )
})

test_that("entropyStats rejects things that are not waves", {
  expect_error(entropyStats("string"), "Expecting a Wave object")
})
