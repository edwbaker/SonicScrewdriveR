#' Convert temperature to Kelvin
#'
#' Converts temperature measurements into Kelvin
#'
#' @param temp The value of the temperature to convert
#' @param input The unit of the temperature to convert, allowed values are "C", "F".
#' @export
#'
convert2Kelvin <- function(temp, input="C") {
  if (input == "C") {
    K <- 273.15 + temp
    validateKelvin(K)
    return(K)
  }
  if (input == "F") {
    K <- (temp - 32) * 5 /9 +273.15
    validateKelvin(K)
    return(K)
  }
  if (input == "K") {
    validateKelvin(temp)
    return(temp)
  }
  stop("Unknown unit: ", input)
}

#' Convert temperature to Celsius
#'
#' Converts temperature measurements into Celsius
#'
#' @param temp The value of the temperature to convert
#' @param input The unit of the temperature to convert, allowed values are "K", "F".
#' @export
#'
convert2Celsius <- function(temp, input="K") {
  if (input == "K") {
    validateKelvin(temp)
    C <- temp - 273.15
    return(C)
  }
  if (input == "F") {
    C <- (temp -32) * 5/9
    validateKelvin(convert2Kelvin(C, input="C"))
    return(C)
  }
  if (input == "C") {
    validateKelvin(convert2Kelvin(temp, input="C"))
    return(temp)
  }
  stop("Unknown unit: ", input)
}

#' Convert temperature to Fahrenheit
#'
#' Converts temperature measurements into Fahrenheit
#'
#' @param temp The value of the temperature to convert
#' @param input The unit of the temperature to convert, allowed values are "K", "C".
#' @export
#'
convert2Fahrenheit <- function(temp, input) {
  stop("Implementation of this function is against the politcial beliefs of the author.")
}
