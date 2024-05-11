#' Normalise a Wave object
#'
#' Similar to normalize() from the tuneR package but automatically identifies the
#' unit parameter.
#'
#' @param wave Wave or WaveMC object
#' @param unit If not null behaves as in normalize() from tuneR, if null the unit
#'   is automatically identified.
#' @param ... Additional arguments passed to normalize() from tuneR
#' @importFrom tuneR normalize
#' @return Normalised Wave or WaveMC object
#' @export
normalise <- function(wave, unit = NULL, ...) {
  if (is.null(unit)) {
    unit <- as.character(wave@bit)
  }
  wave <- normalize(wave, unit = unit, ...)
  return(wave)
}
