#' Generate time masked Wave-like objects
#'
#' Given a `Wave`-like object (or a list of `Wave`-like objects), generate
#' new `Wave`-like objects with time masking.
#' @param wave A `Wave`-like object (or a list of `Wave`-like objects).
#' @param method The method to use for time masking (one of "squarewave", "random).
#' @param dutyCycle The duty cycle of the output. A value of 0.95 means that 5%
#'   of the time is masked.
#' @param n.periods The number of waves to generate in the squarewave method.
#' @return The Wave or WaveMC object with the time mask applied.
#' @export
generateTimeMask <- function(wave, method="squarewave", dutyCycle=0.95, n.periods=10) {
  if (is(wave, "list")) {
    if (!all(sapply(wave, function(x) inherits(x, c("Wave", "WaveMC"))))) {
      stop("wave must be a Wave-like object, or a list of such objects.")
    }
    #Every argument is passed on. Forwarding only the method quietly gave each
    #member of a list the default duty cycle and period count.
    return(lapply(
      wave, generateTimeMask,
      method=method, dutyCycle=dutyCycle, n.periods=n.periods
    ))
  }
  .validateChoice(method, c("squarewave", "random"), "method parameter", "generateTimeMask")

  wl <- length(wave)

  if (method == "squarewave") {
    p <- wl / (n.periods)
    on <- floor(p * dutyCycle)
    off <- ceiling(p - on)
    mask <- rep(c(rep_len(0,off), rep_len(1,on)), n.periods)
    mask <- mask[1:wl]
  }
  if (method == "random") {
    mask <- rep_len(1, wl)
    mask[sample(1:wl, wl*(1-dutyCycle))] <- 0
  }

  if (inherits(wave, "Wave")) {
    wave@left <- wave@left * mask
    if (wave@stereo) {
      wave@right <- wave@right * mask
    }
  }
  if (inherits(wave, "WaveMC")) {
    wave@.Data <- wave@.Data * mask
  }
  return(wave)

}
