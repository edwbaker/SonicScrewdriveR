#' Convert angle to degrees
#'
#' Converts angle measurements into degrees.
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in degrees
#' @examples
#' convert2degrees(pi, input.unit="radians")
#' convert2degrees(90, input.unit="degrees")
#'
convert2degrees <- function(A, input.unit="radians", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2degrees")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .angleUnits(), "convert2degrees")
  return(switch(
    input.unit,
    "degrees" = A,
    "radians" = A * 180 / pi
  ))
}

#' Convert angle to radians
#'
#' Converts angle measurements into radians.
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in radians
#' @examples
#' convert2radians(180, input.unit="degrees")
#' convert2radians(pi, input.unit="radians")
#'
convert2radians <- function(A, input.unit="degrees", input) {
  if (!missing(input)) {
    .deprecatedArg("input", "input.unit", "convert2radians")
    input.unit <- input
  }
  input.unit <- .validateUnit(input.unit, .angleUnits(), "convert2radians")
  return(switch(
    input.unit,
    "radians" = A,
    "degrees" = A * pi / 180
  ))
}
