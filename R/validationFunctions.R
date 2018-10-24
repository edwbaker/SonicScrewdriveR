validateRH <- function(RH) {
  if (RH>100 | RH < 0) {
    stop("Realtive humidity must be between 0 and 100.")
  }
}

validateKelvin <- function(T) {
  if (T < 0) {
    stop("Temperatures must be above 0K.")
  }
}