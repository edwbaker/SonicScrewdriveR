#' Generate time masked Wave-like objects
#'
#' Given a `Wave`-like object (or a list of `Wave`-like objects), generate
#' new `Wave`-like objects with time masking.
#' @param wave A `Wave`-like object (or a list of `Wave`-like objects).
#' @param method The method to use for time masking (one of "squarewave").
#' @export
generateTimeMasked <- function(wave, method="squarewave") {
  if (method == "squarewave") {
    return(wave)
  }
  stop(paste("Unknown method parameter to generateTimeMasked:",method))
}
