#' Generate a frequency-swept sine wave
#'
#' Generates a frequency swept sine wave and returns it as a Wave object or vector.
#'
#' @param f0 Start frequency
#' @param f1 End frequency
#' @param sweep.time Duration of swept wave
#' @param time.unit One of "seconds", "samples"
#' @param A Amplitude of wave
#' @param samp.rate Sample rate of swept wave
#' @param output "wave" for a Wave object, or "vector"
#' @param ... Additional arguments to pass to data2Wave
#' @export
#' @return A swept wave object of the type specified in output.
#' @examples
#' sweptsine()
#'
sweptsine <- function(f0=100, f1=2500, sweep.time=1, time.unit="seconds", A=1, samp.rate=44100, output="wave", ...) {
  if (time.unit == "seconds") {
    vector_length <- samp.rate*sweep.time
  } else if (time.unit == "samples") {
    vector_length <- sweep.time
  } else {
    stop("time.unit must be one of 'seconds' or 'samples'")
  }
  if (!output %in% c("wave", "vector")) {
    stop("output must be one of 'wave' or 'vector'")
  }
  delta_f <- (f1 - f0) / vector_length

  f <- seq(from=f0, by=delta_f, length=vector_length)
  phi <- seq(from=0, by=2*pi*f/samp.rate, length=vector_length)
  w <- A * sin(phi)

  if (output == "vector") {
    return(w)
  }
  if (output == "wave") {
    return(data2Wave(w, samp.rate=samp.rate, ...))
  }

}
