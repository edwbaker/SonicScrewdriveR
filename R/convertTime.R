#' Convert time to seconds
#'
#' Converts time measurements into seconds
#'
#' @param T The time value to convert
#' @param input The unit of time to convert, allowed values are "minutes",
#'   "hours", "days", "years", "HHMM".
#' @export
#' @return The numeric value in seconds
#'
convert2seconds <- function(T, input="minutes") {
  if (input == "seconds") {
    return(as.numeric(T))
  }
  if (input == "minutes") {
    return(T*60)
  }
  if (input == "hours") {
    return(T*60*60)
  }
  if (input == "days") {
    return(T*60*60*24)
  }
  if (input == "years") {
    return(T*60*60*24*365)
  }
  if (input == "HHMM") {
    if (!all(grepl("[[:digit:]]", T))) {
      stop("HHMM input must be numeric")
    }
    return(as.numeric(substr(T,1,2))*60*60 + as.numeric(substr(T,3,4))*60)
  }
  stop(paste("Unknown input to convert2seconds: ",input))
}

#' Converts seconds in human readable form
#'
#' Given an input of bytes calculates the result in a sensible output unit (e.g.
#' minutes, hours).
#'
#' @param S Number of seconds
#' @return String in human readable format
#' @export
#'
humanTime <- function(S) {
  if (S < 60) {
    return(paste(S, "seconds"))
  }
  if (S < 60*60) {
    return (paste(S/60, "minutes"))
  }
  if (S < 60*60*24) {
    return (paste(S/(60*60), "hours"))
  }

  return (paste(S/(60*60*24), "days"))
}
