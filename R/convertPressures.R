#' Convert pressure to Pascals
#'
#' Converts pressure measurements into Pascals
#'
#' @param P The value of the pressure to convert
#' @param input The unit of the pressure to convert, allowed values are "kPa".
#' @export
#' @return The numeric value in Pascals
#' @examples
#' convert2Pascals(1, input="kPa")
#'
convert2Pascals <- function(P, input="kPa") {
  if (input == "kPa") {
    return(P*1000)
  }
  if (input == "Pa") {
    return(P)
  }
}
