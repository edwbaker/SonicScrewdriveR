#' Rainfall detection
#'
#' Detects rainfall in a Wave. An uncalibrated version of Bedoya et al (2017) <doi:10.1016/j.ecolind.2016.12.018> is available in this package. The hardRain
#' package can also be accessed via this wrapper.
#'
#' @param wave A Wave object to detect rainfall in
#' @param method Which rainfall detection method to use ("bedoya2017")
#' @param ... Other arguments to pass to rain detection function
#' @export
#' @return Numeric value from the rainfall detection algorithm chosen.
#' @examples
#' \dontrun{
#' rainfallDetection(sheep, method="bedoya2017")
#' }
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
