#' Calculate the shimmer in a Wave object
#'
#' Shimmer is a measure of the variability of amplitude between successive periods
#' in the waveform. It is returned in decibels, as the mean absolute difference in
#' level between the peak amplitudes of consecutive periods. A waveform of constant
#' amplitude has a shimmer of zero.
#'
#' @param wave A Wave object
#' @export
#' @return The shimmer in decibels
#' @examples
#' \dontrun{
#' shimmer(sheep)
#' }
shimmer <- function(wave) {
    return(shimmer_db(wave))
}

shimmer_db <- function(wave) {
  validateIsWave(wave)

  zc <- .periodBoundaries(wave)
  n <- length(zc) - 1
  if (n < 2) {
    return(NA_real_)
  }

  #The peak amplitude of a period is its largest magnitude. Taking the largest signed
  #value instead would return the value at the zero crossing for negative periods.
  a <- vector(mode="numeric", length=n)
  for (i in 1:n) {
    a[i] <- max(abs(wave@left[zc[i]:zc[i+1]]))
  }

  a2 <- 20 * log10(a[-1] / a[-n])

  #Silent periods give ratios that are not finite, and are treated as no change.
  a2[which(!is.finite(a2))] <- 0

  s <- sum(abs(a2)) / (n-1)

  return(s)
}
