#' Convert pressure to Pascals
#'
#' Converts pressure measurements into Pascals.
#'
#' @param P The value of the pressure to convert
#' @param input.unit The unit of the pressure to convert, one of "Pa", "kPa",
#'   "dyne_cm2".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in Pascals
#' @examples
#' convert2Pascals(1000, input.unit="kPa")
#' convert2Pascals(10, input.unit="dyne_cm2")
#'
convert2Pascals <- function(P, input.unit="kPa", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2Pascals")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .pressureUnits(), "convert2Pascals")
  return(switch(
    input.unit,
    "Pa"       = P,
    "kPa"      = P * 1000,
    "dyne_cm2" = P / 10
  ))
}

#' Convert pressure to kilopascals
#'
#' @param P The value of the pressure to convert
#' @param input.unit The unit of the pressure to convert, one of "Pa", "kPa",
#'   "dyne_cm2".
#' @return The numeric value in kilopascals
#' @keywords internal
#' @noRd
convert2kPascals <- function(P, input.unit="kPa") {
  return(convert2Pascals(P, input.unit) / 1000)
}

#' Convert pressure to dyne per square centimetre
#'
#' Converts pressure measurements into dyne per square centimetre.
#'
#' @param P The value of the pressure to convert
#' @param input.unit The unit of the pressure to convert, one of "Pa", "kPa",
#'   "dyne_cm2".
#' @param input Deprecated, use input.unit instead.
#' @return The numeric value in dyne per square centimetre.
#' @export
#' @examples
#' convert2dyne_cm2(1, input.unit="Pa")
#' convert2dyne_cm2(1, input.unit="kPa")
convert2dyne_cm2 <- function(P, input.unit="kPa", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2dyne_cm2")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .pressureUnits(), "convert2dyne_cm2")
  return(switch(
    input.unit,
    "Pa"       = P * 10,
    "kPa"      = P * 10000,
    "dyne_cm2" = P
  ))
}
