#' Wave to python conversion
#'
#' Helper functions for reticulate to move Wave objects between R and Python
#' @rdname Wave-python-conversion
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
#' @export
py_to_r.Wave <- function(px, convert=FALSE) {
  rx <- Wave(px[[1]], samp.rate = px[[2]])
  return(px)
}
