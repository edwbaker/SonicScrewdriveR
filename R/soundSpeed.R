#' Get the speed of sound in a medium
#'
#' TODO: Description
#'
#' @param medium Propagation medium (default is "air")
#' @param method The method used to calculate speed of sound (default is "vague")
#' @param temperature The temperature used to calculate the sound speed
#' @export
#'
soundSpeed <- function(medium="air",
                       method=NULL,
                       temperature=NULL) {
  if (is.null(method)) {
    if (is.null(temperature)) {
      return(soundSpeed_vague(medium=medium))
    }
  }
}

#' Get the speed of sound in a medium using wavelength and frequency
#'
#' TODO: Description
#'
#' @param wl Wavelength
#' @param f Frequency
#' @export
#'
soundSpeedWF <- function(wl, f) {
  s <- validateWavelength(wl) * validateFreqIsPossible(f)
  return(s)
}

#' Get the speed of sound in a medium using bulk modulus and density
#'
#' TODO: Description
#'
#' @param bm Bulk modulus
#' @param d Density
#' @export
#'
soundSpeedBMD <- function(bm, d) {
  s <- sqrt(validateBulkModulus(bm)/validateDensity(d))
  return(s)
}

soundSpeed_vague <- function(medium="air", temperature=NULL) {
  if (medium == "air") {
    return (343)
  }
  if (medium == "sea water") {
    return(1500)
  }
  stop("No sound speed data for medium: ", medium)
}
