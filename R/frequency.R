#' Get the frequency from wavelength and speed of sound
#'
#' Calculates the frequency of a sound wave given the wavelength and speed of sound in that medium.
#'
#' @param wl Wavelength
#' @param s Speed of sound
#' @examples
#' f <- frequencySound(wl=100, s=343)
#' @export
#'
frequencySound <- function(wl, s) {
  f <- validateWavelength(wl) / validateSpeed(s)
  return(validateFreq(f))
}

naturalFrequency <- function(L, C, R) {
  F_nat <- sqrt((1/(L*C)) - R^2/(4*L^2)) / (2 * pi)
  return(F_nat)
}

resonantFrequency <- function(L, C) {
  F_res <- naturalFrequency(L,C,0)
  return(F_res)
}
