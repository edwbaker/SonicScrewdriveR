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
      warning("pcm set to FALSE since bit=64")
      return(FALSE)
    } else {
      return(pcm)
    }
  }
  if(bit %in% c(8, 16, 24)) {
    if (!pcm) {
      warning("pcm set to TRUE since bit was one of 8, 16, or 24")
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

#' Should a single channel function be applied via allChannels()?
#'
#' Single channel functions read the left slot of a Wave. Anything with more
#' than one channel to consider, and any WaveMC (which has no left slot at all),
#' has to go through allChannels() instead.
#' @noRd
.useAllChannels <- function(wave) {
  if (inherits(wave, "WaveMC")) {
    return(TRUE)
  }
  if (inherits(wave, "Wave")) {
    return(wave@stereo)
  }
  return(FALSE)
}

#' Largest value a wave of a given format may hold
#'
#' Eight bit PCM is unsigned and runs to 255, other PCM depths are signed and run
#' to one less than half their range, and a floating point wave holds values
#' between -1 and 1 whatever its bit slot says. Reading the bit slot alone gives
#' 2^32 for the floating point waves that tuneR creates for bit=1.
#'
#' @param w A Wave or WaveMC object.
#' @return The largest positive sample value the format allows.
#' @noRd
.waveFullScale <- function(w) {
  if (w@bit == 8) {
    return(255)
  }
  if (!w@pcm) {
    return(1)
  }
  return(2^(w@bit - 1) - 1)
}

#' Samples of a Wave-like object as a matrix
#'
#' A Wave holds its channels in the left and right slots, a WaveMC in the columns
#' of a matrix. This returns either as a matrix with one column per channel, so
#' that functions comparing channels do not need to know which they were given.
#'
#' @param wave A Wave or WaveMC object.
#' @return A matrix with one column per channel, named if the channels are named.
#' @noRd
.channelMatrix <- function(wave) {
  if (inherits(wave, "WaveMC")) {
    return(wave@.Data)
  }
  if (wave@stereo) {
    return(cbind(wave@left, wave@right))
  }
  return(matrix(wave@left, ncol=1))
}
