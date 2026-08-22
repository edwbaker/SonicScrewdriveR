#' Convert pressure to Pascals
#'
#' Converts pressure measurements into Pascals
#'
#' @param P The value of the pressure to convert
#' @param input The unit of the pressure to convert, allowed values are "kPa", "dyne_cm2".
#' @export
#' @return The numeric value in Pascals
#' @examples
#' convert2Pascals(1000, input="kPa")
#' convert2Pascals(10, input="dyne_cm2")
#'
convert2Pascals <- function(P, input="kPa") {
  .convertLinear(P, input, "Pa", .pressureMultipliers(), fn="convert2Pascals")
}

convert2kPascals <- function(P, input="kPa") {
  .convertLinear(P, input, "kPa", .pressureMultipliers(), fn="convert2kPascals")
}

#' Convert pressure to dyne per square centimetre
#'
#' Converts pressure measurements into dyne per square centimetre
#'
#' @param P The value of the pressure to convert
#' @param input The unit of the pressure to convert, allowed values are "Pa", "kPa".
#' @return The numeric value in dyne per square centimetre.
#' @export
#' @examples
#' convert2dyne_cm2(1, input="Pa")
#' convert2dyne_cm2(1, input="kPa")
convert2dyne_cm2 <- function(P, input="kPa") {
  .convertLinear(P, input, "dyne_cm2", .pressureMultipliers(), fn="convert2dyne_cm2")
}
