#' Reference pressure
#'
#' Provides the standard reference pressure level.
#'
#' @param output.unit Unit to return, one of "Pa", "dyne_cm2".
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
  output.unit <- .validateUnit(
    output.unit, c("Pa", "dyne_cm2"), "referencePressure", "output.unit"
  )
  return(switch(
    output.unit,
    "dyne_cm2" = 0.0002,
    "Pa"       = convert2Pascals(0.0002, "dyne_cm2")
  ))
}

#' Reference intensity
#'
#' Provides the standard reference intensity level.
#'
#' @param output.unit Unit to return, "watt_cm2".
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
  output.unit <- .validateUnit(
    output.unit, c("watt_cm2"), "referenceIntensity", "output.unit"
  )
  return(switch(
    output.unit,
    "watt_cm2" = 10^-16
  ))
}
