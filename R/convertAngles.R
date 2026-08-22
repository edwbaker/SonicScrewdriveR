#' Convert angle to degrees
#'
#' Converts angle measurements into degrees
#'
#' @param A The angle value to convert
#' @param input The unit of angle to convert, allowed values are "radians".
#' @export
#' @return The numeric value in degrees
#'
convert2degrees <- function(A, input="radians") {
  .convertLinear(A, input, "degrees", .angleMultipliers(), fn="convert2degrees")
}

#' Convert angle to radians
#'
#' Converts angle measurements into radians
#'
#' @param A The angle value to convert
#' @param input The unit of angle to convert, allowed values are "degrees".
#' @export
#' @return The numeric value in radians
#'
convert2radians <- function(A, input="degrees") {
  .convertLinear(A, input, "radians", .angleMultipliers(), fn="convert2radians")
}
