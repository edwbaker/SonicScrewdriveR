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


test_that("a spectrum of all zeros is returned unnormalised with a warning", {
  #max() of it is zero, so dividing by it gave a spectrum of NaN that
  #validateSpectrum() then turned into these same zeros without a word.
  z <- cbind(0:4, rep(0, 5))
  expect_warning(out <- normaliseSpectrum(z), "all zero")
  expect_equal(out, z)

  #zeroSpectrum() builds exactly such a spectrum from a real one.
  wn <- tuneR::noise("white")
  wns <- seewave::spec(wn, plot=FALSE)
  expect_warning(out <- normaliseSpectrum(zeroSpectrum(wns)), "all zero")
  expect_equal(out[, 2], rep(0, nrow(wns)))

  #Amplitudes that are only negative are coerced to zero before the largest is
  #taken, so they reach the same case rather than normalising against a
  #negative maximum.
  expect_warning(out <- normaliseSpectrum(cbind(0:4, rep(-1, 5))), "all zero")
  expect_equal(out[, 2], rep(0, 5))
})

test_that("a spectrum with any amplitude above zero still normalises", {
  wn <- tuneR::noise("white")
  wns <- seewave::spec(wn, plot=FALSE)
  expect_silent(out <- normaliseSpectrum(wns))
  expect_equal(max(out[, 2]), 1)

  one <- cbind(0:4, c(0, 0, 2, 0, 0))
  expect_silent(out <- normaliseSpectrum(one))
  expect_equal(out[, 2], c(0, 0, 1, 0, 0))
})

test_that("spectrum arithmetic reports NaN amplitudes rather than zeroing them", {
  #Every amplitude of an all-NaN spectrum used to be replaced with zero, so
  #adding two of them returned a silent spectrum as though it were the sum.
  nan <- cbind(0:4, rep(NaN, 5))
  expect_error(addSpectra(nan, nan), "No NaN values allowed in spectra.")
  expect_error(subtractSpectra(nan, nan), "No NaN values allowed in spectra.")
  expect_error(zeroSpectrum(nan), "No NaN values allowed in spectra.")
  expect_error(normaliseSpectrum(nan), "No NaN values allowed in spectra.")
})
