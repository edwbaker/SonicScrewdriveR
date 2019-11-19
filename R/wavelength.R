wavelength <- function(frequency, speed=soundSpeedMedium("air"), unit="m") {
  if (unit == "m") {
    return (speed/frequency)
  }
  if (unit == "cm") {
    return (100 * speed/frequency)
  }
  stop("Invalid unit selction: ", unit)
}

