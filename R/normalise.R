#' Normalise a Wave object
#'
#' Similar to normalize() from the tuneR package but automatically identifies the
#' unit parameter.
#'
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()]. The unit that
#' was used is recorded, rather than the `NULL` that may have been asked for.
#'
#' @param wave Wave or WaveMC object
#' @param unit If not null behaves as in normalize() from tuneR, if null the unit
#'   is automatically identified.
#' @param ... Additional arguments passed to normalize() from tuneR
#' @importFrom tuneR normalize
#' @return Normalised Wave or WaveMC object
#' @export
normalise <- function(wave, unit = NULL, ...) {
  #Nothing was checked here, so anything with a bit slot was passed on to tuneR
  #and anything without it failed on the slot. .tagApplyOne() expects a wave.
  validateIsWaveLike(wave)
  if (is.null(unit)) {
    unit <- as.character(wave@bit)
  }
  return(.tagApplyOne(wave, .normalise, "normalise", NULL, TRUE, unit = unit, ...))
}

#' Normalise a wave without recording it
#'
#' The wave is named with a dot for the same reason `.tagApplyOne()`'s arguments
#' are: everything meant for normalize() arrives in `...`, and R would match a
#' short name against the formals before it.
#' @keywords internal
#' @noRd
.normalise <- function(.wave, ...) {
  return(normalize(.wave, ...))
}
