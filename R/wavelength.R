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
  if (!missing(unit)) {
    .deprecatedArg("unit", "output.unit", "wavelength")
    output.unit <- unit
  }
  .convertLinear(
    speed/frequency, "m", output.unit, .wavelengthMultipliers(),
    msg=paste0("Invalid unit selection: ", output.unit)
  )
}

