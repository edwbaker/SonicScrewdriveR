#' Get the speed of sound in a medium
#'
#' TODO: Description
#'
#' @param medium Propagation medium (default is "air")
#' @param method The method used to calculate speed of sound (default is "vague")
#' @param temperature The temperature used to calculate the sound speed
#' @export
#'
soundSpeed <- function(medium="air",
                       method=NULL,
                       temperature=NULL) {
  if (is.null(method)) {
    if (is.null(temperature)) {
      return(soundSpeed_vague(medium=medium))
    }
  }
}

soundSpeedBMD <- function(bm, d) {
  s <- sqrt(validateBM(bm)/validateDensity(d))
  return(s)
}

soundSpeed_vague <- function(medium="air", temperature=NULL) {
  if (medium == "air") {
    return (343)
  }
  stop("No sound speed data for medium: ", medium)
}
