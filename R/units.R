# Unit handling
#
# The package names unit arguments consistently:
#
#   input.unit   the unit the supplied value is in
#   output.unit  the unit the returned value is in
#   <quantity>.unit  where a function takes several quantities that each
#                    need a unit (e.g. temp.unit, pressure.unit)
#
# The units recognised for each quantity are listed by the registry functions
# below, which are also used to generate error messages, so that the accepted
# set is defined in exactly one place.

#' Units recognised for pressure
#'
#' @return A character vector of recognised pressure units.
#' @keywords internal
#' @noRd
.pressureUnits <- function() {
  return(c("Pa", "kPa", "dyne_cm2"))
}

#' Units recognised for temperature
#'
#' @return A character vector of recognised temperature units.
#' @keywords internal
#' @noRd
.temperatureUnits <- function() {
  return(c("K", "C", "F"))
}

#' Units recognised for angles
#'
#' @return A character vector of recognised angle units.
#' @keywords internal
#' @noRd
.angleUnits <- function() {
  return(c("degrees", "radians"))
}

#' Units recognised for file sizes
#'
#' @return A character vector of recognised file size units.
#' @keywords internal
#' @noRd
.fileSizeUnits <- function() {
  return(c("bits", "bytes", "kB", "MB", "GB"))
}

#' Units recognised for lengths
#'
#' @return A character vector of recognised length units.
#' @keywords internal
#' @noRd
.lengthUnits <- function() {
  return(c("m", "cm"))
}

#' Units recognised for positions within a cycle
#'
#' @return A character vector of recognised cyclical position units.
#' @keywords internal
#' @noRd
.fractionUnits <- function() {
  return(c("radians", "fraction"))
}

#' Units recognised for positions within a wave
#'
#' @return A character vector of recognised wave position units.
#' @keywords internal
#' @noRd
.wavePositionUnits <- function() {
  return(c("samples", "seconds", "minutes", "hours"))
}

#' Validate a unit argument
#'
#' Checks a unit argument against the units recognised for a quantity. If the
#' unit is not recognised an error is raised listing the units that are, so
#' that the accepted set never has to be looked up in the documentation.
#'
#' @param unit The unit supplied by the caller.
#' @param allowed A character vector of recognised units, usually from one of
#'   the registry functions in this file.
#' @param fun The name of the calling function, used in the error message.
#' @param arg The name of the argument being validated, used in the error
#'   message.
#' @return The validated unit.
#' @keywords internal
#' @noRd
.validateUnit <- function(unit, allowed, fun, arg="input.unit") {
  if (!is.character(unit) || length(unit) != 1 || is.na(unit)) {
    stop(arg, " of ", fun, "() must be a single unit name. Valid units are: ",
         paste(allowed, collapse=", "), call.=FALSE)
  }
  if (!unit %in% allowed) {
    stop("Unknown ", arg, " to ", fun, "(): ", unit,
         ". Valid units are: ", paste(allowed, collapse=", "), call.=FALSE)
  }
  return(unit)
}

#' Warn that an argument has been renamed
#'
#' Used by the deprecation shims that keep the former names of renamed unit
#' arguments working.
#'
#' @param old The deprecated argument name.
#' @param new The argument that replaces it.
#' @param fun The function the argument belongs to.
#' @return No return value, called for its side effect of raising a warning.
#' @keywords internal
#' @noRd
.deprecatedArg <- function(old, new, fun) {
  warning("The ", old, " argument of ", fun, "() is deprecated and will be ",
          "removed in a future release, use ", new, " instead.", call.=FALSE)
  return(invisible(NULL))
}
