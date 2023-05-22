#' Reference pressure
#'
#' Provides the standard reference pressure level.
#'
#' @param unit Unit to return, "Pa" or "dyne_cm2"
#' @examples
#' rp <- referencePressure()
#' rp <- referencePressure(unit="dyne_cm2")
#' @export
#'
referencePressure <- function(unit="Pa") {
  if (unit=="Pa") {
    return(convert2Pascals(0.0002, input="dyne_cm2"))
  }
  if (unit=="dyne_cm2") {
    return(0.0002)
  }
}

#' Reference intensity
#'
#' Provides the standard reference intensity level.
#'
#' @param unit Unit to return, "watt_cm2"
#' @examples
#' ri <- referenceIntensity()
#' @export
#'
referenceIntensity <- function(unit="watt_cm2") {
  if (unit=="watt_cm2") {
    return(10^-16)
  }
}