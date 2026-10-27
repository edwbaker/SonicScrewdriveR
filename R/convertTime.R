#' Convert time to seconds
#'
#' Converts time measurements into seconds.
#'
#' @param t The time value to convert
#' @param input.unit The unit of time to convert, one of "seconds", "minutes",
#'   "hours", "days", "years", "HHMM", "POSIX".
#' @param origin For POSIX whether to return relative to start of day ("day")
#'   or Unix epoch ("unix")
#' @param T Deprecated, use t instead.
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in seconds
#' @examples
#' convert2seconds(1, input.unit="minutes")
#' convert2seconds("0130", input.unit="HHMM")
#'
convert2seconds <- function(t, input.unit="minutes", origin="day", T, input) {
  if (!missing(T)) {
    .deprecatedArg("T", "t", "convert2seconds")
    t <- T
  }
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2seconds")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .convertable2seconds(), "convert2seconds")
  s <- switch(
    input.unit,
    "seconds" = as.numeric(t),
    "minutes" = as.numeric(t) * 60,
    "hours"   = as.numeric(t) * 60 * 60,
    "days"    = as.numeric(t) * 60 * 60 * 24,
    "years"   = as.numeric(t) * 60 * 60 * 24 * 365,
    "HHMM"    = .HHMM2seconds(t),
    "POSIX"   = .POSIX2seconds(t, origin)
  )
  return(validateTimeInSeconds(s))
}

#' Units of time convertible to seconds
#'
#' @return A character vector of recognised time units.
#' @keywords internal
#' @noRd
.convertable2seconds <- function() {
  return(c("seconds", "minutes", "hours", "days", "years", "HHMM", "POSIX"))
}

#' Convert an HHMM time to seconds since the start of the day
#'
#' @param t A time in HHMM form, as a number or a string.
#' @return The numeric value in seconds.
#' @keywords internal
#' @noRd
.HHMM2seconds <- function(t) {
  # TODO: Validate hours and minutes
  if (!all(grepl("[[:digit:]]", t))) {
    stop("HHMM input must be numeric")
  }
  # Pad start with 0 to length 4
  t <- sprintf("%04d", as.numeric(t))
  return(as.numeric(substr(t, 1, 2)) * 60 * 60 + as.numeric(substr(t, 3, 4)) * 60)
}

#' Convert a POSIX time to seconds
#'
#' @param t An object that can be coerced to POSIXlt.
#' @param origin Whether to count from the start of the day ("day") or from the
#'   Unix epoch ("unix").
#' @return The numeric value in seconds.
#' @keywords internal
#' @noRd
.POSIX2seconds <- function(t, origin="day") {
  if (!is.character(origin) || length(origin) != 1 || !origin %in% c("day", "unix")) {
    stop("origin of convert2seconds() must be one of: day, unix", call.=FALSE)
  }
  if (origin == "day") {
    lt <- unclass(as.POSIXlt(t))
    return(lt$sec + 60 * lt$min + 3600 * lt$hour)
  }
  return(as.numeric(t))
}

#' Converts time to human readable form
#'
#' Given an input of seconds calculates the result in a sensible output unit
#' (e.g. minutes, hours).
#'
#' @param S Time to convert, in input.unit
#' @param input.unit The unit of time to convert, one of "seconds", "minutes",
#'   "hours", "days", "years", "HHMM", "POSIX".
#' @param unit Deprecated, use input.unit instead.
#' @return String in human readable format
#' @export
#'
humanTime <- function(S, input.unit="seconds", unit) {
  if (!missing(unit)) {
    .deprecatedArg("unit", "input.unit", "humanTime")
    input.unit <- unit
  }
  S <- convert2seconds(S, input.unit)
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
