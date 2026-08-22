#' Sample duration
#'
#' Calculates the time represented by n samples in a Wave.
#'
#' @param n The number of the samples
#' @param wave A Wave object containing pulses
#' @param samp.rate Integer sampling rate
#' @export
#' @return A numeric value in seconds
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
  samp.rate <- .resolveSampRate(wave, samp.rate)
  duration <- n / samp.rate
  return(duration)
}

#' Samples per time period
#'
#' Calculates the number of samples for a given duration of a wave
#'
#' @param time The duration in seconds
#' @param wave A Wave object containing pulses. Its sample rate is used, so it
#'   cannot be given alongside samp.rate.
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
  samp.rate <- .resolveSampRate(wave, samp.rate)
  #Multiplied directly. Dividing by sDuration(), which is one over the sample
  #rate, put a second rounding in the way and left floor() a sample short of an
  #exact duration.
  return(floor(round(time * samp.rate, 9)))
}

#' The sample rate to work from
#'
#' A wave carries its own sample rate, so giving one as well says two things at
#' once. Passing both used to discard the sample rate without saying so, where
#' validateFreqIsPossible() treats the same pair as an error.
#'
#' @param wave A Wave-like object, or NULL.
#' @param samp.rate A sample rate, or NULL.
#' @return The sample rate to use.
#' @noRd
.resolveSampRate <- function(wave, samp.rate) {
  if (is.null(wave) && is.null(samp.rate)) {
    stop("samp.rate or wave must be specified")
  }
  if (!is.null(wave) && !is.null(samp.rate)) {
    stop("Give either a wave or a samp.rate, not both")
  }
  if (!is.null(wave)) {
    return(wave@samp.rate)
  }
  return(samp.rate)
}
