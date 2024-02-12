#' Convert time to seconds
#'
#' Converts time measurements into seconds
#'
#' @param T The time value to convert
#' @param input The unit of time to convert, allowed values are "minutes",
#'   "hours", "days", "years", "HHMM".
#' @param origin For POSIX whether to return relative to start of day ("day") or Unix epoch ("unix")
#' @export
#' @return The numeric value in seconds
#'
convert2seconds <- function(T, input="minutes", origin="day") {
  if (!input %in% .convertable2seconds()) {
    stop(paste("Unknown input to convert2seconds:",input))
  }
  if (input == "seconds") {
    s <- as.numeric(T)
  }
  if (input == "minutes") {
    s <- as.numeric(T)*60
  }
  if (input == "hours") {
    s <- as.numeric(T)*60*60
  }
  if (input == "days") {
    s <- as.numeric(T)*60*60*24
  }
  if (input == "years") {
    s <- as.numeric(T)*60*60*24*365
  }
  if (input == "HHMM") {
    # TODO: Validate hours and minutes
    if (!all(grepl("[[:digit:]]", T))) {
      stop("HHMM input must be numeric")
    }
    s <- as.numeric(substr(T,1,2))*60*60 + as.numeric(substr(T,3,4))*60
  }
  if (input == "POSIX") {
    if (origin == "day") {
      t <- unclass(as.POSIXlt(T))
      s <- (t$sec + 60*t$min + 3600*t$hour)
    }
    if (origin == "unix") {
      s <- as.numeric(T)
    }
  }
  return(validateTimeInSeconds(s))
}

.convertable2seconds <- function() {
  return(c("seconds", "minutes", "hours", "days", "years", "HHMM", "POSIX"))
}

#' Converts time to human readable form
#'
#' Given an input of bytes calculates the result in a sensible output unit (e.g.
#' minutes, hours).
#'
#' @param S Time to convert in unit
#' @param unit The unit of time to convert
#' @return String in human readable format
#' @export
#'
humanTime <- function(S, unit="seconds") {
  S <- convert2seconds(S, unit)
  if (S < 60) {
    if (S == 1) {
      return("1 second")
    } else {
      return(paste(S, "seconds"))
    }
  }
  if (S < 60*60) {
    if (S/60 == 1) {
      return(paste(S/60, "minute"))
    } else {
      return(paste(S/60, "minutes"))
    }
  }
  if (S < 60*60*24) {
    if (S/(60*60) == 1) {
      return(paste(S/(60*60), "hour"))
    } else {
      return(paste(S/(60*60), "hours"))
    }
  }
  if (S/(60*60*24) == 1) {
    return(paste(S/(60*60*24), "day"))
  }
  return(paste(S/(60*60*24), "days"))
}
