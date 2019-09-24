#' Rainfal detection
#'
#' Detects rainfall in a Wave.
#' 
#' @param wave A Wave object to detect rainfall in
#' @param method Which rainfall detection method to use
#' @param ... Other arguments to pass to pulse detection function
#' @export
#'
rainfallDetection <- function(
  wave,
  method="bedoya2017",
  ...
) {
  if (method == "bedoya2017") {
    return(rainfall_bedoya2017(wave,...))
  }
  stop("No valid method supplied.")
}
