mkSpectrum <- function(seed, n=20) {
  set.seed(seed)
  return(matrix(c(seq(0, 10, length.out=n), runif(n)), ncol=2))
}

spectra <- lapply(1:5, mkSpectrum)

test_that("specStats returns a ggplot without drawing it", {
  expect_s3_class(specStats(spectra), "ggplot")
  expect_s3_class(specStats(spectra, stats="sd"), "ggplot")
})

test_that("specStats accepts a single spectrum", {
  # Previously the validation loop ran over 2:length(spectra), which for one
  # spectrum indexed spectra[[2]] and gave "subscript out of bounds".
  expect_s3_class(specStats(spectra[1]), "ggplot")
})

test_that("specStats rejects bad input", {
  expect_error(specStats(spectra, stats="dog"), "Unknown stats for specStats: dog")
  expect_error(specStats(list()), "requires a list of one or more spectra")
  expect_error(specStats(mkSpectrum(1)), "requires a list of one or more spectra")
  expect_error(specStats(spectra[1], stats="sd"), "Two or more spectra are required")
})

test_that("specStats rejects spectra that are not comparable", {
  other <- matrix(c(seq(0, 20, length.out=20), runif(20)), ncol=2)
  expect_error(specStats(list(spectra[[1]], other)), "same frequency bins")
})

test_that("specStats statistics match a direct calculation", {
  amp <- vapply(spectra, function(s) s[,2], numeric(20))

  mm <- specStats_min_max(spectra)
  expect_equal(colnames(mm), c("min", "max", "mean"))
  expect_equal(unname(mm[,"min"]), apply(amp, 1, min))
  expect_equal(unname(mm[,"max"]), apply(amp, 1, max))
  expect_equal(unname(mm[,"mean"]), rowMeans(amp))

  s <- specStats_sd(spectra)
  expect_equal(colnames(s), c("sd", "mean"))
  expect_equal(unname(s[,"sd"]), apply(amp, 1, stats::sd))
  expect_equal(unname(s[,"mean"]), rowMeans(amp))
})
