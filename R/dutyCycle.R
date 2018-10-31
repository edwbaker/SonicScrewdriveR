#' Calculate the duty cycle of a wave
#'
#' Proportion of a wave with signal above the limit
#' 
#' @param wave A Wave object 
#' @param limit Threshold above which to consider the signal
#' @export
#'
dutyCycle <- function(
  wave,
  limit=0.1
) {
  wave <- normalize(wave)
  w <- abs(wave@left)
  l <- length(w)
  c <- sum(w > limit*max(w))
  return(c/l)
}