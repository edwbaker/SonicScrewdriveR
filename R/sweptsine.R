#' Generate a frequency-swept sine wave
#'
#' Generates a frequency swept sine wave and returns it as a Wave object or vector.
#'
#' @param f0 Start frequency
#' @param f1 End frequency
#' @param sweep.time Duration of swept wave
#' @param time.unit One of "seconds", "samples"
#' @param samp.rate Sample rate of swept wave
#' @param output "wave" for a Wave object, or "vector"
#' @param ... Additional arguments to pass to data2Wave
#' @export
#' @return A swept wave object of the type specified in output.
#' @examples
#' #Generate a swept sine wave between 0Hz and 10kHz.
#' w <- sweptsine(0, 10e3)
#'
#' #Generate a swept sine wave between 0Hz and 10kHz and normalise it.
#' w <- normalise(sweptsine(0, 10e3))
#'
#' #Generate a stereo swept sine wave between 100Hz and 1KHz.
#' w <- stereo(sweptsine(100, 1e3))
#'
sweptsine <- function(f0=100, f1=2500, sweep.time=1, time.unit="seconds", samp.rate=44100, output="wave", ...) {
  if (!output %in% c("wave", "vector")) {
    stop("output must be one of 'wave' or 'vector'")
  }
  if (f1 <= f0) {
    stop("sweptsine: f1 must be greater than f0")
  }
  if (time.unit == "seconds") {
    vector_length <- samp.rate*sweep.time
  } else if (time.unit == "samples") {
    vector_length <- sweep.time
  } else {
    stop("time.unit must be one of 'seconds' or 'samples'")
  }

  delta_f <- (f1 - f0) / vector_length

  f <- seq(from=f0, by=delta_f, length=vector_length)
  phi <- seq(from=0, by=pi*f/samp.rate, length=vector_length)
  w <- sin(phi)

  if (output == "vector") {
    return(w)
  }
  if (output == "wave") {
    return(data2Wave(w, samp.rate=samp.rate, remove.offset=FALSE, ...))
  }
}
