#' Identify zero crossings in a Wave object
#'
#' Returns a vector of the position (in samples) of zero crossings in
#' a Wave object
#'
#' @param wave A Wave object
#' @export
#' @return A vector of zero crossing locations
#' @examples
#' \dontrun{
#' zerocross(sheep)
#' }

zerocross <- function(wave) {
  validateIsWave(wave)
  #A sample that is exactly zero is only a crossing if the signal changes sign
  #across it. Every zero used to be reported as one, so a run of them gave a
  #crossing per sample and a zero between two samples of the same sign gave a
  #crossing that never happened.
  nonzero <- which(wave@left != 0)
  if (length(nonzero) == 0) {
    return(integer(0))
  }

  zc <- integer(0)
  if (length(nonzero) > 1) {
    changes <- which(diff(sign(wave@left[nonzero])) != 0)
    before <- nonzero[changes]
    after <- nonzero[changes + 1]
    #Where zeroes lie between the two samples the first of them is the crossing,
    #and otherwise the sample after the sign changed, which matches real time.
    zc <- ifelse(after - before > 1, before + 1, after)
  }

  #A wave that begins at zero and then departs from it crosses at its first
  #sample. There is nothing before it to change sign against, so it has to be
  #taken separately, and a wave that is zero throughout is handled above.
  if (wave@left[1] == 0) {
    zc <- c(1L, zc)
  }
  return(zc)
}

#' Identify the period boundaries in a Wave object
#'
#' A period spans two zero crossings, so every second crossing is taken to give one
#' boundary per period. Using every crossing would measure half cycles instead, and
#' the two halves of an asymmetric waveform differ even when its period is perfectly
#' constant.
#'
#' @param wave A Wave object
#' @return A vector of period boundary locations
#' @noRd
.periodBoundaries <- function(wave) {
  return(zerocross(wave)[c(TRUE, FALSE)])
}
