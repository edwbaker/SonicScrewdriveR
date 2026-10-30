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
  #The message was built by pasting output.unit into it, which repeated the
  #whole message once per element when more than one unit was given. The shared
  #wording names the argument and the function, as every other converter does.
  .convertLinear(
    speed/frequency, "m", output.unit, .wavelengthMultipliers(),
    what="output.unit", fn="wavelength"
  )
}

