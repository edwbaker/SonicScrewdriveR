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
  #Get locations of zero-crossings
  az <- which(wave@left == 0) #Actual zeroes

  wave@left[az] <- NA         #Prevent double-detection of zero crossings where actual zeroes occur
  zc <- which(diff(sign(wave@left)) != 0) + 1 #+1 places zc at start of sample after crossing, to match real time
  zc <- sort(c(az,zc))
  wave@left[az] <- 0
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
