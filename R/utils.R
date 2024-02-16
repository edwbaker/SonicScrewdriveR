.equalWaveTR <- utils::getFromNamespace("equalWave", "tuneR")

#' Tests if two Wave-like objects have the same properties
#'
#' @param w1 a Wave-like object
#' @param w2 a Wave-like object
#' @return a logical value
#' @keywords internal
#' @noRd
.equalWave <- function(w1, w2) {
  return(.equalWaveTR(untagWave(w1), untagWave(w2)))
}
