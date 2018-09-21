#' Windowing Function for Wave Objects
#'
#' Seperates a Wave object into windows of a defined length and runs a function on the window section. Windows may overlap, and the function can make use of 'parallel' package for multicore processing.
#' 
#' @param wave A Wave object
#' @param window.length The lag used to create the A-matrix
#' @param window.overlap A matrix used to code the Duration-Shape pairs
#' @param max_D The maximum Duration to code
#' @param FUN If TRUE plots the workings of the coding algorithm
#' @param ... Additional parameters to FUN
#' @param cluster A cluser form the 'parallel' package for multicore computation
#' @keywords wave
#' @export
#' @examples
#' library(tuneR)
#' wave <- readWave(system.file("extdata", "1.wav", package="tdsc"))
#' t <- tdsc(wave)

windowing <- function(wave, window.length, window.overlap=0, FUN, ..., cluster=NULL) {
  n.windows <- ceiling(length(wave@left) / (window.length - window.overlap))
  starts <- c(
    1,
    1:(n.windows-1) * (window.length - window.overlap) +1
  )
  
  if (is.null(cluster)){
    l <- lapply(starts, FUN, wave=wave, window.length=window.length, ...)
  } else {
    l <- parLapply(cluster, starts, FUN, wave=wave, window.length=window.length, ...)
  }
  
  return(l)
}

pwl <- function(start, wave=NULL, window.length=NULL) {
  remaining <- min(window.length, length(wave) + 1 - start) -1
  w <- wave@left[start:(start+remaining)]
  s <- length(w)
  return(s)
}
