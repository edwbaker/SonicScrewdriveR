#' Reference pressure
#'
#' Provides the standard reference pressure level.
#'
#' @param output.unit Unit to return, "Pa" or "dyne_cm2"
#' @param unit Deprecated, use output.unit instead.
#' @examples
#' rp <- referencePressure()
#' rp <- referencePressure(output.unit="dyne_cm2")
#' @return The reference sound pressure, in the unit specified.
#' @export
#'
referencePressure <- function(output.unit="Pa", unit) {
  if (!missing(unit)) {
    .deprecatedArg("unit", "output.unit", "referencePressure")
    output.unit <- unit
  }
  if (!output.unit %in% c("Pa", "dyne_cm2")) {
    stop(paste("Unknown unit to referencePressure:",output.unit))
  }
  if (output.unit=="Pa") {
    return(convert2Pascals(0.0002, input.unit="dyne_cm2"))
  }
  if (output.unit=="dyne_cm2") {
    return(0.0002)
  }
}

#' Reference intensity
#'
#' Provides the standard reference intensity level.
#'
#' @param output.unit Unit to return, "watt_cm2"
#' @param unit Deprecated, use output.unit instead.
#' @examples
#' ri <- referenceIntensity()
#' @return The reference sound intensity, in the unit specified.
#' @export
#'
referenceIntensity <- function(output.unit="watt_cm2", unit) {
  if (!missing(unit)) {
    .deprecatedArg("unit", "output.unit", "referenceIntensity")
    output.unit <- unit
  }
  if (!output.unit %in% c("watt_cm2")) {
    stop(paste("Unknown unit to referenceIntensity:",output.unit))
  }
  if (output.unit=="watt_cm2") {
    return(10^-16)
  }
}
