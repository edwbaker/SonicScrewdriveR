#' Rainfall detection
#'
#' Detects rainfall in a Wave. An uncallibrated version of Bedoya (REF) is available in this package. The hardRain
#' package can also be accessed via this wrapper.
#'
#' @param wave A Wave object to detect rainfall in
#' @param method Which rainfall detection method to use ("bedoya2017", "hardRain")
#' @param ... Other arguments to pass to rain detection function
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
  if (method == "hardRain") {
    package.installed("hardRain", "github", "Cdevenish")
    return(rainfall_hardRain(wave,...))
  }
  stop("No valid method supplied.")
}
