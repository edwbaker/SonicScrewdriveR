#' Convert temperature to Kelvin
#'
#' Converts temperature measurements into Kelvin
#'
#' @param temp The value of the temperature to convert
#' @param input.unit The unit of the temperature to convert, one of "K", "C",
#'   "F".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return Numeric value in Kelvin
#' @examples
#' convert2Kelvin(15, input.unit="C")
#' convert2Kelvin(15, input.unit="F")
#'
convert2Kelvin <- function(temp, input.unit="C", input) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(input="input.unit"), "convert2Kelvin"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  if (input.unit == "C") {
    K <- 273.15 + temp
    validateKelvin(K)
    return(K)
  }
  if (input.unit == "F") {
    K <- (temp - 32) * 5 /9 +273.15
    validateKelvin(K)
    return(K)
  }
  if (input.unit == "K") {
    validateKelvin(temp)
    return(temp)
  }
  stop("Unknown unit: ", input.unit)
}

#' Convert temperature to Celsius
#'
#' Converts temperature measurements into Celsius
#'
#' @param temp The value of the temperature to convert
#' @param input.unit The unit of the temperature to convert, one of "K", "C",
#'   "F".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return Numeric value in degrees Celsius
#' @examples
#' convert2Celsius(15, input.unit="K")
#' convert2Celsius(15, input.unit="F")
#'
convert2Celsius <- function(temp, input.unit="K", input) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(input="input.unit"), "convert2Celsius"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  if (input.unit == "K") {
    validateKelvin(temp)
    C <- temp - 273.15
    return(C)
  }
  if (input.unit == "F") {
    C <- (temp -32) * 5/9
    validateKelvin(convert2Kelvin(C, input.unit="C"))
    return(C)
  }
  if (input.unit == "C") {
    validateKelvin(convert2Kelvin(temp, input.unit="C"))
    return(temp)
  }
  stop("Unknown unit: ", input.unit)
}

#' Convert temperature to Fahrenheit
#'
#' Converts temperature measurements into Fahrenheit
#'
#' @param temp The value of the temperature to convert
#' @param input.unit The unit of the temperature to convert, one of "K", "C",
#'   "F".
#' @param input Deprecated, use input.unit instead.
#' @return No return value, this conversion is not implemented and the function raises an error.
#' @export
#' @examples
#' \dontrun{
#' convert2Fahrenheit(15, input.unit = "C")
#' }
#'
convert2Fahrenheit <- function(temp, input.unit, input) {
  stop("Implementation of this function is against the political beliefs of the author.")
}
