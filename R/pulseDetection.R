#' Pulse detection
#'
#' Detects pulses in a Wave, defaults to using Dietrich (2004).
#' 
#' @param wave A Wave object containing pulses
#' @param method Which method to use for pulse detection
#' @param ... Other arguments to pass to pulse detection function
#' @export
#'
pulseDetection <- function(
  wave,
  method="dietrich2004",
  ...
) {
  if (method == "dietrich2004") {
    return(pd_dietrich2004(wave,...))
  }
  stop("No valid method supplied.")
}