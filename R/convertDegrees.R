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
  if (input == "radians") {
    return(A*180/pi)
  }
  stop(paste("Unknown input to convert2degrees: ",input))
}
