#' Get the speed of sound in a medium
#'
#' Provides typical values of the speed of sound in a given medium (air,
#' sea water, freshwater).
#'
#' @param medium Propagation medium (default is "air"). "all" gives a list of all
#'   available media.
#' @return Typical value of the speed of sound in m/s for the medium
#' @keywords internal
#' @noRd
#' @examples
#' soundSpeedMedium("air")
#' soundSpeedMedium("sea water")
#'
.soundSpeedMedium <- function(medium="air") {
  names  <- c("air", "sea water", "freshwater", "helium", "hydrogen", "liquid helium", "mercury", "aluminium", "lead", "steel")
  values <- c(343, 1500, 1430, 999, 1330, 211, 1451, 6420, 1960, 5941)
  if (medium=="all") {
    return(data.frame(cbind(names, values)))
  }
  if (medium %in% names) {
    return(values[which(names==medium)])
  }
  stop("No sound speed data for medium: ", medium)
}

#' Calculate the speed of sound in a medium
#'
#' Given sufficient parameters (i.e. wavelength and frequency, bulk modulus and density) this
#' function calculates the speed of sound.
#'
#' @param medium Propagation medium (e.g. "air")
#' @param wl Wavelength
#' @param f Frequency
#' @param bulkModulus Bulk modulus
#' @param density Density
#' @export
#'
soundSpeed <- function(medium=NULL, wl=NULL, f=NULL, bulkModulus=NULL, density=NULL) {
  # If all arguments are null set default
  if (all(is.null(medium), is.null(wl), is.null(f), is.null(bulkModulus), is.null(density))) {
    medium <- "air"
  }
  if (!is.null(medium)) {
    s <- .soundSpeedMedium(medium)
    return(s)
  }
  if (!is.null(wl) & !is.null(f)) {
    s <- validateWavelength(wl) * validateFreq(f)
    return(s)
  }
  if (!is.null(bulkModulus) & !is.null(density)) {
    s <- sqrt(validateBulkModulus(bulkModulus)/validateDensity(density))
    return(s)
  }
}
