#' Get the speed of sound in a medium
#'
#' Provides typical values of the speed of sound in a given medium (air, sea water, freshwater).
#'
#' @param medium Propagation medium (default is "air")
#' @export
#' @return Typical value of the speed of sound in m/s for the medium
#' @examples
#' soundSpeedMedium("air")
#' soundSpeedMedium("sea water")
#'
soundSpeedMedium <- function(medium="air") {
  if (medium == "air") {
    return (343)
  }
  if (medium == "sea water") {
    return(1500)
  }
  if (medium == "freshwater") {
    return(1430)
  }
  stop("No sound speed data for medium: ", medium)
}

#' Calculate the speed of sound in a medium
#'
#' Given sufficient parameters (i.e. wavelength and frequency, bulk modulus and density) this
#' function calculates the speed of sound.
#'
#' @param wl Wavelength
#' @param f Frequency
#' @param bulkModulus Bulk modulus
#' @param density Density
#' @export
#'
soundSpeed <- function(wl=NULL, f=NULL, bulkModulus=NULL, density=NULL) {
  if (!is.null(wl) & !is.null(f)) {
    s <- validateWavelength(wl) * validateFreq(f)
    return(s)
  }
  if (!is.null(bulkModulus) & !is.null(density)) {
    s <- sqrt(validateBulkModulus(bulkModulus)/validateDensity(density))
    return(s)
  }
}
