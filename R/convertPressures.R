#' Convert pressure to Pascals
#'
#' Converts pressure measurements into Pascals
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
  .convertLinear(P, input.unit, "Pa", .pressureMultipliers(),
                 what="input.unit", fn="convert2Pascals")
}

convert2kPascals <- function(P, input.unit="kPa") {
  .convertLinear(P, input.unit, "kPa", .pressureMultipliers(),
                 what="input.unit", fn="convert2kPascals")
}

#' Convert pressure to dyne per square centimetre
#'
#' Converts pressure measurements into dyne per square centimetre
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
  .convertLinear(P, input.unit, "dyne_cm2", .pressureMultipliers(),
                 what="input.unit", fn="convert2dyne_cm2")
}
