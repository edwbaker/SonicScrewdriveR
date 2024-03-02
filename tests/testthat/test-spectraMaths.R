test_that("Addition and subtraction works as expected", {
  wn <- tuneR::noise("white")
  wns <- seewave::spec(wn, plot=FALSE)
  expect_equal(wns, addSpectra(wns, zeroSpectrum(wns)))
  expect_equal(wns[,2]*2, addSpectra(wns, wns)[,2])
  expect_equal(wns[,2], subtractSpectra(wns, zeroSpectrum(wns))[,2])
})

test_that("normalise Spectra works", {
  wn <- tuneR::noise("white")
  wns <- seewave::spec(wn, plot=FALSE)
  wns[1,2] <- 42
  wnsn <- normaliseSpectrum(wns)
  expect_equal(max(wnsn[,2]), 1)
})

