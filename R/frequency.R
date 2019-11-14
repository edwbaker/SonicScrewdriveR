#' Get the frequency from wavelength and speed of sound
#'
#' Calculates the frequency of a sound wave given the wavelength and speed of sound in that medium.
#'
#' @param wl Wavelength
#' @param s Speed of sound
#' @return Frequency of the sound in Hertz
#' @examples
#' f <- frequencySound(wl=100, s=343)
#' @export
#'
frequencySound <- function(wl, s) {
  f <- validateWavelength(wl) / validateSpeed(s)
  return(validateFreq(f))
}
