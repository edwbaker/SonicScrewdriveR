#' Convert angle to degrees
#'
#' Converts angle measurements into degrees
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in degrees
#'
convert2degrees <- function(A, input.unit="radians", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2degrees")
    input.unit <- input
  }
  .convertLinear(A, input.unit, "degrees", .angleMultipliers(),
                 what="input.unit", fn="convert2degrees")
}

#' Convert angle to radians
#'
#' Converts angle measurements into radians
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in radians
#'
convert2radians <- function(A, input.unit="degrees", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2radians")
    input.unit <- input
  }
  .convertLinear(A, input.unit, "radians", .angleMultipliers(),
                 what="input.unit", fn="convert2radians")
}
