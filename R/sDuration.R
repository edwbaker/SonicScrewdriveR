#' Sample duration
#'
#' Calculates the time represenetd by n samples in a Wave.
#'
#' @param n The number of the samples
#' @param wave A Wave object containing pulses
#' @param samp.rate Ineteger sampling rate
#' @export
#'
#'
sDuration <- function(
  n = 1,
  wave = NULL,
  samp.rate = NULL
){
  if (is.null(wave) & is.null(samp.rate)){
    stop("samp.rate or wave must be specified")
  }
  if (!is.null(wave)) {
    samp.rate <- wave@samp.rate
  }
  duration <- n / samp.rate
  return(duration)
}

#' Samples per time period
#'
#' Calculates the number of samples for a given duration of a wave
#'
#' @param time The duration in seconds
#' @param wave A Wave object containing pulses
#' @param samp.rate Ineteger sampling rate
#' @export
#'
#'
tSamples <- function(
  time = 1,
  wave = NULL,
  samp.rate = NULL
){
  if (is.null(wave) & is.null(samp.rate)){
    stop("samp.rate or wave must be specified")
  }
  if (!is.null(wave)) {
    samp.rate <- wave@samp.rate
  }
  n <- time / sDuration(wave=wave)
  return(floor(n))
}
