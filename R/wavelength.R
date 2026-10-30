#' Get the wavelength from frequency and speed of sound
#'
#' Calculates the wavelength of a sound wave given the frequency and speed of sound in that medium.
#'
#' @param frequency Frequency of the sound in Hertz
#' @param speed Speed of sound (defaults to the speed of sound in air)
#' @param output.unit Unit to return the wavelength in, either "m" or "cm"
#' @param unit Deprecated, use output.unit instead.
#' @return Wavelength of the sound in the unit specified
#' @examples
#' wl <- wavelength(1000)
#' wl <- wavelength(1000, speed=soundSpeed(medium="steel"))
#' wl <- wavelength(1000, output.unit="cm")
#' @export
#'
wavelength <- function(frequency, speed=soundSpeed(medium="air"), output.unit="m",
                       unit) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(unit="output.unit"), "wavelength"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  #frequencySound(), the same relationship the other way round, validates both
  #its input and its result. This validated neither, so a negative frequency
  #gave a negative wavelength and a frequency of zero gave Inf, each of which
  #went on into whatever consumed it. The zero is refused here rather than in
  #validateFreq(), whose bound is inclusive because the first bin of a spectrum
  #is 0 Hz.
  validateFreq(frequency)
  if (any(!is.na(frequency) & frequency == 0)) {
    stop("Frequency must be greater than zero to have a wavelength.")
  }
  wl <- .convertLinear(
    validateSpeed(speed)/frequency, "m", output.unit, .wavelengthMultipliers(),
    what="output.unit", fn="wavelength"
  )
  return(validateWavelength(wl))
}

