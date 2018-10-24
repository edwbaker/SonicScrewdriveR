#' Sample duration
#'
#' Calculates the time represenetd by n samples in a Wave.
#' 
#' @param n The number of the samples
#' @param wave A Wave object containing pulses
#' @param samp.rate Ineteger sampling rate
#' @param ... Other arguments to pass to pulse detection function
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