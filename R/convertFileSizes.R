#' Convert bits to bytes
#'
#' Converts file size measurements into bytes.
#'
#' @param S The value to convert
#' @param input.unit The unit to convert, one of "bits", "bytes", "kB", "MB",
#'   "GB".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in bytes
#' @examples
#' convert2bytes(8, input.unit="bits")
#' convert2bytes(1, input.unit="MB")
#'
convert2bytes <- function(S, input.unit="bits", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2bytes")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .fileSizeUnits(), "convert2bytes")
  return(switch(
    input.unit,
    "bits"  = S / 8,
    "bytes" = S,
    "kB"    = S * 1e+3,
    "MB"    = S * 1e+6,
    "GB"    = S * 1e+9
  ))
}

#' Converts bytes in human readable form
#'
#' Given an input of bytes calculates the result in a sensible output unit (e.g.
#' MB, GB, PB).
#'
#' @param S Number of bytes
#' @return String in human readable format
#' @export
#'
humanBytes <- function(S) {
  if (S >= 1e+18) {
    S <- S/1e+18
    return(paste(S,"EB"))
  } else if (S >= 1e+15) {
    S <- S/1e+15
    return(paste(S, "PB"))
  } else if (S >= 1e+12) {
    S <- S/1e+12
    return(paste(S, "TB"))
  } else if (S >= 1e+9) {
    S <- S/1e+9
    return(paste(S, "GB"))
  } else if (S >= 1e+6) {
    S <- S/1e+6
    return(paste(S, "MB"))
  } else if (S >= 1e+3) {
    S <- S/1e+3
    return(paste(S, "kB"))
  } else {
    if (S==1) {
      return(paste(S, "byte"))
    } else {
      return(paste(S, "bytes"))
    }
  }
}
