#' Pulse detection
#'
#' Detects pulses in a Wave. Stereo and multi-channel waves are analysed one
#' channel at a time, giving one result per channel.
#' 
#' @param wave A Wave object containing pulses
#' @param method Which method to use for pulse detection
#' @param ... Other arguments to pass to pulse detection function
#' @return The output of the pulse detection method used.
#' @export
#'
pulseDetection <- function(
  wave,
  method="simple",
  ...
) {
  #The methods below all read the left slot, so anything with more than one
  #channel goes through allChannels(), as dutyCycle() and rainfallDetection() do.
  #Stereo waves were otherwise analysed on the left channel alone, without saying
  #so, and a WaveMC has no left slot at all.
  if (.useAllChannels(wave)) {
    return(allChannels(
      wave,
      function(w) pulseDetection(w, method=method, ...),
      channel.param = NULL
    ))
  }
  .validateChoice(
    method, c("dietrich2004", "simple", "threshold"),
    msg="No valid method supplied."
  )
  if (method == "dietrich2004") {
    return(pd_dietrich2004(wave,...))
  }
  if (method == "simple") {
    return(pd_simple(wave,...))
  }
  return(pd_threshold(wave, ...))
}