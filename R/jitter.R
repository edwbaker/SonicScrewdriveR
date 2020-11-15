#' Calculate the jitter in a Wave object
#'
#' Jitter is a measure of the variability of periods in the waveform. Relative
#' jitter is scaled by the jitter in the analysed waveform.
#'
#' @param wave A Wave object
#' @param method One of "absolute" or "relative"
#' @export
#' @return A vector of zero crossing locations
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
  zc <- zerocross(wave)
  t <- diff(zc)
  n <- length(t)

  j <- sum(abs(diff(t))) / (n - 1)
  return(j)
}

jitter_rel <- function(wave) {
  zc <- zerocross(wave)
  t <- diff(zc)
  n <- length(t)

  j <- sum(abs(diff(t))) / (n - 1)
  j <- j/mean(t)
  return(j)
}

#jitter_rap

#jitter_ppq5

