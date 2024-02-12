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
  if (input == "degrees"){
    return(A)
  }
  stop(paste("Unknown input to convert2degrees:",input))
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
  if (input == "degrees") {
    return(A*pi/180)
  }
  if (input == "radians"){
    return(A)
  }
  stop(paste("Unknown input to convert2radians:",input))
}
