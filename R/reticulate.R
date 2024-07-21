#' Wave to python conversion
#'
#' Helper functions for reticulate to move Wave objects between R and Python
#' @rdname Wave-python-conversion
#' @param rx A Wave object.
#' @param convert Logical indicating whether to convert the Wave object to a Python object.
#' @export
r_to_py.Wave<- function(rx, convert=FALSE) {
  # Check rx is a Wave object
  if (inherits(rx, "Wave")) {
    if (rx@stereo) {
      #ToDo: Handle Stereo
    } else {
      validateIsWave(rx)
      px <- list(as.numeric(unlist(rx@left)), rx@samp.rate)
    }
  } else if (inherits(rx, "WaveMC")) {
    #ToDO: Handle WaveMC
  }
  #assign("convert", convert, envir = px)
  return(px)
}

#' @rdname Wave-python-conversion
#' @param px A Python representation of a wave.
#' @param convert Logical indicating whether to convert the Wave object to an R object.
#' @export
py_to_r.Wave <- function(px, convert=FALSE) {
  rx <- Wave(px[[1]], samp.rate = px[[2]])
  return(px)
}
