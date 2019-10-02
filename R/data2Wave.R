#' Convert data into a Wave object
#'
#' TODO: Description
#'
#' @param left Data for  audio channel
#' @param samp.rate Sampling rate for Wave object
#' @param bit Bit depth of Wave object
#' @export
#'
data2Wave <- function(left, samp.rate=44100, bit=16) {
  d <- as.numeric(left)
  a <- mean(d)
  d <- d-a
  wave <- tuneR::Wave(left=d, right = numeric(0), samp.rate=samp.rate, bit=bit)
  wave <- tuneR::normalize(wave)
  return(wave)
}