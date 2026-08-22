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
  .validateChoice(input, .convertable2seconds(), "input", "convert2seconds")
  factors <- .secondMultipliers()
  if (input %in% names(factors)) {
    s <- as.numeric(T) * factors[[input]]
  }
  if (input == "HHMM") {
    # TODO: Validate hours and minutes
    #Anchored, so that input which merely contains a digit is rejected here with
    #the intended message rather than failing later with an unrelated one.
    if (!all(grepl("^[[:digit:]]{1,4}$", T))) {
      stop("HHMM input must be numeric")
    }
    # Pad start with 0 to length 4
    T <- sprintf("%04d", as.numeric(T))
    s <- as.numeric(substr(T,1,2))*60*60 + as.numeric(substr(T,3,4))*60
  }
  if (input == "POSIX") {
    .validateChoice(origin, c("day", "unix"), "origin", "convert2seconds")
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
  return(c(names(.secondMultipliers()), "HHMM", "POSIX"))
}

#' Seconds in each unit that convert2seconds() takes as a plain factor
#'
#' The HHMM and POSIX inputs are parsed rather than scaled, so they are not here.
#'
#' @return A named numeric vector.
#' @noRd
.secondMultipliers <- function() {
  return(c(seconds=1, minutes=60, hours=60*60, days=60*60*24, years=60*60*24*365))
}

#' Number of seconds in each unit used by humanTime()
#'
#' @return A named numeric vector of the number of seconds in each unit, from
#'   smallest to largest.
#' @noRd
.timeUnitMultipliers <- function() {
  ret <- c(60, 60*60, 60*60*24)
  names(ret) <- c("minute", "hour", "day")
  return(ret)
}

#' Converts time to human readable form
#'
#' Given a time calculates the result in a sensible output unit (e.g. minutes,
#' hours).
#'
#' @param S Time to convert in unit. A vector may be given.
#' @param unit The unit of time to convert
#' @param digits Number of decimal places to round to, or NULL for no rounding.
#' @return String in human readable format, one for each value of S.
#' @export
#' @examples
#' humanTime(90)
#' humanTime(c(1, 90, 7200))
#' humanTime(1, unit="hours")
#'
humanTime <- function(S, unit="seconds", digits=3) {
  S <- convert2seconds(S, unit)
  return(.humanUnits(S, .timeUnitMultipliers(), "second", pluralise="all", digits=digits))
}
