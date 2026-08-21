#' Get the wavelength from frequency and speed of sound
#'
#' Calculates the wavelength of a sound wave given the frequency and speed of sound in that medium.
#'
#' @param frequency Frequency of the sound in Hertz
#' @param speed Speed of sound (defaults to the speed of sound in air)
#' @param unit Unit to return the wavelength in, either "m" or "cm"
#' @return Wavelength of the sound in the unit specified
#' @examples
#' wl <- wavelength(1000)
#' wl <- wavelength(1000, speed=soundSpeed(medium="steel"))
#' wl <- wavelength(1000, unit="cm")
#' @export
#'
wavelength <- function(frequency, speed=soundSpeed(medium="air"), unit="m") {
  if (unit == "m") {
    return (speed/frequency)
  }
  if (unit == "cm") {
    return (100 * speed/frequency)
  }
  stop("Invalid unit selection: ", unit)
}

