#' Calculate the duty cycle of a wave
#'
#' Proportion of a wave with signal above the limit
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which returns a list with one duty cycle per channel.
#' @param limit Threshold above which to consider the signal
#' @param output If "unit" the duty cycle will be in the range 0-1. For a percentage use "percent".
#' @param normalise If TRUE the Wave is normalised using tuneR
#' @return A numerical value for the duty cycle between 0 and 1 (or 0 and 100% if
#'   percentage output). For multi-channel input, a list of such values.
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
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(
      wave,
      function(w) dutyCycle(w, limit=limit, output=output, normalise=normalise),
      channel.param = NULL
    ))
  }
  if (normalise) {
    wave <- normalise(wave)
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
