#' Get the frequency from wavelength and speed of sound
#'
#' Calculates the frequency of a sound wave given the wavelength and speed of sound in that medium.
#'
#' @param wl Wavelength
#' @param s Speed of sound (defaults to the speed of sound in air)
#' @examples
#' f <- frequencySound(wl=100, s=343)
#' @export
#'
frequencySound <- function(wl, s=soundSpeedMedium("air")) {
  f <- validateWavelength(wl) / validateSpeed(s)
  return(validateFreq(f))
}
