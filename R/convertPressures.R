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
  if (input == "kPa") {
    return(P*1000)
  }
  if (input == "dyne_cm2") {
    return(P/10)
  }
  if (input == "Pa") {
    return(P)
  }
  stop(paste("Unknown input to convert2Pascals:",input))
}

convert2kPascals <- function(P, input="kPa") {
  return(convert2Pascals(P, input)/1000)
}

#' Convert pressure to dyne per square centimetre
#'
#' Converts pressure measurements into dyne per square centimetre
#'
#' @param P The value of the pressure to convert
#' @param input The unit of the pressure to convert, allowed values are "kPa", "P".
#' @export
#' @examples
#' convert2dyne_cm2(1, input="Pa")
#' convert2dyne_cm2(1, input="kPa")
convert2dyne_cm2 <- function(P, input="kPa") {
  if (input == "kPa") {
    return(P*10000)
  }
  if (input == "dyne_cm2") {
    return(P)
  }
  if (input == "Pa") {
    return(P*10)
  }
  stop(paste("Unknown input to convert2dyne_cm2:",input))
}
