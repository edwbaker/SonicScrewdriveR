#' Calculate the shimmer in a Wave object
#'
#' Jitter is a measure of the variability of amplitudes within periods in the waveform. Relative
#' shimmer is scaled by the shimmer in the analysed waveform.
#'
#' @param wave A Wave object
#' @export
#' @return A vector of zero crossing locations
#' @examples
#' \dontrun{
#' shimmer(sheep)
#' }
shimmer <- function(wave) {
    return(shimmer_db(wave))
}

shimmer_db <- function(wave) {
  validateIsWave(wave)
  zc <- zerocross(wave)
  t <- diff(zc)
  n <- length(t)

  a <- vector(mode="numeric", length=length(zc)-1)
  for (i in 1:length(a)) {
    a[i] <- max(wave@left[zc[i]:zc[i+1]])
  }


  a2 <- vector(mode="numeric", length=length(a))
  for (i in 1:(length(a2)-1)) {
    a2[i] <- a[i+1] / a[i]
  }

  a2 <- 20 *log10(a2)

  a2[which(is.infinite(a2))] <- 0

  s <- sum(abs(a2)) / (n-1)

  return(s)
}
