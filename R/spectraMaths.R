#' Add two spectra from seewave
#'
#' This function takes two spectra from seewave (or equivalent) and adds their values. The spectra must have
#' the same bins.
#'
#' @param s1 First spectrum
#' @param s2 Second spectrum
#' @param coerceNegative Sets any values below zero to zero in output.
#' @return A spectrum of s1+s2
#' @export
#' @examples
#' \dontrun{
#' subtractSpectra(spec1, spec2)
#' }
#'
addSpectra <- function(s1, s2, coerceNegative=TRUE) {
  validateComparableSpectra(s1, s2)
  s1 <- validateSpectrum(s1, coerceNegative=coerceNegative)
  s2 <- validateSpectrum(s2, coerceNegative=coerceNegative)
  s1[,2] <- s1[,2] + s2[,2]
  s1 <- validateSpectrum(s1, coerceNegative=coerceNegative)
  return(s1)
}

#' Subtract two spectra from seewave
#'
#' This function takes two spectra from seewave (or equivalent) and subtracts their values. The spectra must have
#' the same bins.
#'
#' @param s1 First spectrum
#' @param s2 Second spectrum
#' @param coerceNegative Sets any values below zero to zero in output.
#' @return A spectrum of s1 - s2
#' @export
#' @examples
#' \dontrun{
#' subtractSpectra(spec1, spec2)
#' subtractSpectra(spec1, spec2, coerceNegative=TRUE)
#' }
#'
subtractSpectra <- function(s1, s2, coerceNegative=TRUE) {
  validateComparableSpectra(s1, s2)
  s1 <- validateSpectrum(s1, coerceNegative=coerceNegative)
  s2 <- validateSpectrum(s2, coerceNegative=coerceNegative)
  s1[,2] <- s1[,2] - s2[,2]
  s1 <- validateSpectrum(s1, coerceNegative=coerceNegative)
  return(s1)
}

#' Zero spectrum
#'
#' This function takes a spectrum from seewave and creates a new zero-valued spectrum with the same structure.
#'
#' @param s1 Spectrum to emulate the structure of.
#' @return A zero-valued spectrum.
#' @export
#' @examples
#' \dontrun{
#' zeroSpectrum(spec)
#' }
#'
zeroSpectrum <- function(s1) {
  s1<- validateSpectrum(s1, coerceNegative = TRUE)
  s1[,2] <- rep_len(0, nrow(s1))
  return(s1)
}

normaliseSpectrum <- function(s1) {
  s1<- validateSpectrum(s1, coerceNegative = TRUE)
  m <- max(s1[,2])
  s1[,2] <- s1[,2]/m
  return(s1)
}
