#' Pulse detection
#'
#' Detects pulses in a Wave.
#' 
#' @param wave A Wave object
#' @param method Which method to use
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