
r_to_py.Wave<- function(rx, convert) {
  # Check rx is a Wave object
  if (inherits(rx, "Wave")) {
    if (rx@stereo) {
      #ToDo: Handle Stereo
    } else {
      validateIsWave(rx)
      px <- reticulate::tuple(rx@left, rx@samp.rate, convert=convert)
    }
  } else if (inherits(rx, "WaveMC")) {
    #ToDO: Handle WaveMC
  }
  return(px)
}

py_to_wave <- function(px) {
  rx <- Wave(px[[1]], samp.rate = px[[2]])
  return(px)
}
