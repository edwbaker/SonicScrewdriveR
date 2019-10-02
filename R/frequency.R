#' Get the frequency from wavelength and speed of sound
#'
#' TODO: Description
#'
#' @param wl Wavelength
#' @param s Speed of sound
#' @export
#'
frequencyWS <- function(wl, s) {
  f <- validateWavelength(wl) / validateSpeed(s)
  return(validateFreqIsPossible(f))
}