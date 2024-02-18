#' @importFrom Rdpack reprompt

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

#' Check PCM given bit and pcm
#'
#' @param bit bit depth (see `tuneR`).
#' @param pcm logical value (see `tuneR`).
#' @return a logical value
#' @keywords internal
#' @noRd
.setPCM <- function(bit, pcm) {
  # Modified from tuneR
  if (bit == 1) {
    # Pass through to tuneR
    return(pcm)
  }
  if(bit == 64) {
    if (pcm){
      warning("pcm set to FALSE since unit=64")
      return(FALSE)
    } else {
      return(pcm)
    }
  }
  if(bit %in% c(8, 16, 24)) {
    if (!pcm) {
      warning("pcm set to TRUE since unit was one of 8, 16, or 24")
      return(TRUE)
    } else {
      return(pcm)
    }
  }
  if(bit == 32) {
    return(pcm)
  }
  stop("bit must be one of 8, 16, 24, 32, or 64.")
}
