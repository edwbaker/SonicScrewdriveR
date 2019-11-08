#' Windowing Function for Wave Objects
#'
#' Separates a Wave object into windows of a defined length and runs a function on the window section. Windows may overlap, and the function can make use of 'parallel' package for multicore processing.
#'
#' @param wave A Wave object
#' @param window.length The lag used to create the A-matrix
#' @param window.overlap A matrix used to code the Duration-Shape pairs
#' @param bind.wave If TRUE and FUN returns wave objects these are combined into a single object
#' @param FUN If TRUE plots the workings of the coding algorithm
#' @param ... Additional parameters to FUN
#' @param cluster A cluster form the 'parallel' package for multicore computation
#' @keywords wave
#' @export
#' @examples
#' \dontrun{
#' windowing(wave, window.length=1000, window.overlap=0, bind.wave=TRUE, FUN=noChange)
#' }

windowing <- function(
  wave,
  window.length,
  window.overlap=0,
  bind.wave=TRUE,
  FUN,
  ...,
  cluster=NULL){
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

  if (typeof(l[[1]]) == "S4" & class(l[[1]])[[1]] == "Wave") {
    w <- l[[1]]
    for (i in 2:length(l)) {
      w <- tuneR::bind(w, l[[i]])
    }
    l <- w
  }
  return(l)
}



pwl <- function(start, wave=NULL, window.length=NULL) {
  remaining <- min(window.length, length(wave) + 1 - start) -1
  w <- wave@left[start:(start+remaining)]
  s <- length(w)
  return(s)
}

noChange <- function(start, wave, window.length) {
  remaining <- min(window.length, length(wave) + 1 - start) -1
  w <- tuneR::Wave(wave@left[start:(start+remaining)], samp.rate=wave@samp.rate, bit=wave@bit)
  return(w)
}
