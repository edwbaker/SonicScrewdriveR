#' Calculate the duty cycle of a wave
#'
#' Proportion of a wave with signal above the limit
#'
#' @param wave A Wave object
#' @param limit Threshold above which to consider the signal
#' @param output If "unit" the duty cycle will be in the range 0-1. For a percentage use "percent".
#' @param normalise If TRUE the Wave is normalised using tuneR
#' @return A numerical value for the duty cycle between 0 and 1 (or 0 and 100% if percentage output).
#' @examples
#' wave <- tuneR::sine(2000)
#' dc <- dutyCycle(wave)
#' pc <- dutyCycle(wave, output="percent")
#' @export
#'
dutyCycle <- function(
  wave,
  limit=0.1,
  output="unit",
  normalise = TRUE
) {
  if (normalise) {
    wave <- tuneR::normalize(wave)
  }
  w <- abs(wave@left)
  l <- length(w)
  c <- sum(w > limit*max(w))
  if (output == "unit") {
    return(validateDutyCycle(c/l))
  }
  if (output == "percent") {
    return(100*validateDutyCycle(c/l))
  }
}
