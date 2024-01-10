scaleRGB <- function(vector, col) {
  vector <- as.numeric(vector)
  vector <- (255 / (max(vector) - min(vector))) * (vector - min(vector))
  return(as.integer(vector))
}

#' Map three vectors to RGB
#'
#' Maps three vectors of equal length to RGB for use in flase colour index
#' spectrograms
#' @param red The red channel vector
#' @param green The green channel vector
#' @param blue The blue channel vector
#' @return A vecotr of RGB values
#' @export
map2RGB <- function(red, green, blue) {
  red <- scaleRGB(red)
  blue <- scaleRGB(blue)
  green <- scaleRGB(green)
  return(rgb(red, green, blue, maxColorValue=255))
}
