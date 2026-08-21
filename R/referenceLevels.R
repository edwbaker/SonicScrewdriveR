#' Reference pressure
#'
#' Provides the standard reference pressure level.
#'
#' @param unit Unit to return, "Pa" or "dyne_cm2"
#' @examples
#' rp <- referencePressure()
#' rp <- referencePressure(unit="dyne_cm2")
#' @return The reference sound pressure, in the unit specified.
#' @export
#'
referencePressure <- function(unit="Pa") {
  if (!unit %in% c("Pa", "dyne_cm2")) {
    stop(paste("Unknown unit to referencePressure:",unit))
  }
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
#' @return The reference sound intensity, in the unit specified.
#' @export
#'
referenceIntensity <- function(unit="watt_cm2") {
  if (!unit %in% c("watt_cm2")) {
    stop(paste("Unknown unit to referenceIntensity:",unit))
  }
  if (unit=="watt_cm2") {
    return(10^-16)
  }
}
