#' Identify zero crossings in a Wave object
#'
#' Returns a vector of the position (in samples) of zero crossings in
#' a Wave object
#'
#' @param wave A Wave object
#' @export
#' @return A vector of zero crossing locations
#' @examples
#' \dontrun{
#' zerocross(sheep)
#' }

zerocross <- function(wave) {
  #Get locations of zero-crossings
  az <- which(wave@left == 0) #Actual zeroes

  wave@left[az] <- NA         #Prevent double-detection of zero crossings where actual zeroes occur
  zc <- which(diff(sign(wave@left)) != 0) + 1 #+1 places zc at start of sample after crossing, to match real time
  zc <- sort(c(az,zc))
  wave@left[az] <- 0
  return(zc)
}
