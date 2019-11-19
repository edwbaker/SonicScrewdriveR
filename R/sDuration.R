#' Sample duration
#'
#' Calculates the time represented by n samples in a Wave.
#'
#' @param n The number of the samples
#' @param wave A Wave object containing pulses
#' @param samp.rate Integer sampling rate
#' @export
#' @return A numeirc value in seconds
#' @examples
#' sDuration(n=20, samp.rate=44100)
#' \dontrun{
#' sDuration(n=20, wave=sheep)#'
#' }
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
#' @param samp.rate Integer sampling rate
#' @export
#' @return Number of samples
#' @examples
#' tSamples(10, samp.rate=44100)
#' \dontrun{
#' tSamples(10, wave=sheep)
#' }
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
  n <- time / sDuration(samp.rate=samp.rate)
  return(floor(n))
}
