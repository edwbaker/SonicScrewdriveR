#' Convert time to seconds
#'
#' Converts time measurements into seconds
#'
#' @param t The time value to convert
#' @param input.unit The unit of time to convert, one of "seconds", "minutes",
#'   "hours", "days", "years", "HHMM", "POSIX".
#' @param origin For POSIX whether to return relative to start of day ("day") or Unix epoch ("unix")
#' @param timezone The timezone to read a POSIX time in, which decides when its
#'   day starts. The default, NULL, uses the timezone the time itself carries,
#'   and UTC for a time that carries none (such as the output of `Sys.time()`).
#' @param T Deprecated, use t instead.
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in seconds
#'
convert2seconds <- function(t, input.unit="minutes", origin="day", timezone=NULL,
                            T, input) {
  if (!missing(T)) {
    .deprecatedArg("T", "t", "convert2seconds")
    t <- T
  }
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2seconds")
    input.unit <- input
  }
  .validateChoice(input.unit, .convertable2seconds(), "input.unit", "convert2seconds")
  factors <- .secondMultipliers()
  if (input.unit %in% names(factors)) {
    s <- as.numeric(t) * factors[[input.unit]]
  }
  if (input.unit == "HHMM") {
    # TODO: Validate hours and minutes
    #Anchored, so that input which merely contains a digit is rejected here with
    #the intended message rather than failing later with an unrelated one.
    if (!all(grepl("^[[:digit:]]{1,4}$", t))) {
      stop("HHMM input must be numeric")
    }
    # Pad start with 0 to length 4
    t <- sprintf("%04d", as.numeric(t))
    s <- as.numeric(substr(t,1,2))*60*60 + as.numeric(substr(t,3,4))*60
  }
  if (input.unit == "POSIX") {
    .validateChoice(origin, c("day", "unix"), "origin", "convert2seconds")
    if (origin == "day") {
      #Which day a moment falls in, and how far through it, depends on the zone
      #it is read in. Breaking it down without naming one read a time carrying
      #no timezone in the session's, so the answer moved with the machine.
      lt <- unclass(as.POSIXlt(.asInstant(t), tz=.timezoneOf(t, timezone)))
      s <- (lt$sec + 60*lt$min + 3600*lt$hour)
    }
    if (origin == "unix") {
      s <- as.numeric(t)
    }
  }
  return(validateTimeInSeconds(s))
}

#A time already broken down into its parts is returned unchanged by
#as.POSIXlt(), whatever timezone that is asked for, so a POSIXlt ignored the
#timezone it was given. Turning it back into an instant first, by the zone it
#carries, leaves it to be read in the zone asked for like any other. A string
#is left alone, as as.POSIXlt() reads it in the zone named rather than in the
#session's, which is what keeps a written-out time the same everywhere.
.asInstant <- function(t) {
  if (inherits(t, "POSIXlt")) {
    return(as.POSIXct(t))
  }
  return(t)
}

#Which timezone to read a time in: the one the caller named, else the one the
#time carries, else UTC. A POSIXct built from an instant, as Sys.time() and
#as.POSIXct() on a plain number both are, carries no timezone, and reading one
#of those without naming a zone is what let the session's zone decide.
.timezoneOf <- function(t, timezone=NULL) {
  if (!is.null(timezone)) {
    return(timezone)
  }
  tz <- attr(t, "tzone")
  if (is.null(tz) || !nzchar(tz[[1]])) {
    return("UTC")
  }
  return(tz[[1]])
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
#' @param S Time to convert, in input.unit. A vector may be given.
#' @param input.unit The unit of time to convert
#' @param digits Number of decimal places to round to, or NULL for no rounding.
#' @param unit Deprecated, use input.unit instead.
#' @return String in human readable format, one for each value of S.
#' @export
#' @examples
#' humanTime(90)
#' humanTime(c(1, 90, 7200))
#' humanTime(1, input.unit="hours")
#'
humanTime <- function(S, input.unit="seconds", digits=3, unit) {
  if (!missing(unit)) {
    .deprecatedArg("unit", "input.unit", "humanTime")
    input.unit <- unit
  }
  S <- convert2seconds(S, input.unit)
  return(.humanUnits(S, .timeUnitMultipliers(), "second", pluralise="all", digits=digits))
}
