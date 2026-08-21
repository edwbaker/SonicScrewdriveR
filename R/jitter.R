#' Calculate the jitter in a Wave object
#'
#' Jitter is a measure of the variability of the length of successive periods in the
#' waveform. Absolute jitter is the mean absolute difference between the lengths of
#' consecutive periods, in samples; relative jitter divides this by the mean period
#' length. A waveform of constant period has a jitter of zero.
#'
#' Periods are located from zero crossings, so they are measured to the nearest
#' sample. A tone whose period is not a whole number of samples will report a small
#' amount of jitter for this reason.
#'
#' @param wave A Wave object
#' @param method One of "absolute" or "relative"
#' @export
#' @return The jitter, in samples for method "absolute" and as a proportion of the
#'   mean period for method "relative"
#' @examples
#' \dontrun{
#' jitter(sheep, method="absolute")
#' jitter(sheep, method="relative")
#' }
jitter <- function(wave, method="absolute") {
  if (method=="absolute") {
    return(jitter_abs(wave))
  }
  if (method=="relative") {
    return(jitter_rel(wave))
  }
}

jitter_abs <- function(wave)  {
  validateIsWave(wave)
  t <- diff(.periodBoundaries(wave))
  n <- length(t)
  if (n < 2) {
    return(NA_real_)
  }

  j <- sum(abs(diff(t))) / (n - 1)
  return(j)
}

jitter_rel <- function(wave) {
  validateIsWave(wave)
  t <- diff(.periodBoundaries(wave))
  n <- length(t)
  if (n < 2) {
    return(NA_real_)
  }

  j <- sum(abs(diff(t))) / (n - 1)
  j <- j/mean(t)
  return(j)
}


