#' Calculate the duty cycle of a wave
#'
#' Proportion of a wave with signal above the limit
#' 
#' @param wave A Wave object 
#' @param limit Threshold above which to consider the signal
#' @param output If "unit" the duty cycle will be in the range 0-1. For a percntage use "percent".
#' @export
#'
dutyCycle <- function(
  wave,
  limit=0.1,
  output="unit"
) {
  wave <- tuneR::normalize(wave)
  w <- abs(wave@left)
  l <- length(w)
  c <- sum(w > limit*max(w))
  if (output == "unit") {
    return(c/l)
  }
  if (output == "percent") {
    return(100*c/l)
  }
}