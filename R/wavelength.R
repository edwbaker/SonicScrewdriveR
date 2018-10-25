wavelength <- function(frequency, speed=NULL, unit="m") {
  if (is.null(speed)) {
    warning("No speed set for wavelength calculation, relying on value for air (343m/s).")
    speed <-343
  }
  if (unit == "m") {
    return (speed/frequency)
  }
  if (unit == "cm") {
    return (100 * speed/frequency)
  }
  stop("Invalid unit selction: ", unit)
}

