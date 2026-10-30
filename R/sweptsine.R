#' Generate a sine sweep
#'
#' Generates a frequency swept sine wave (either linear or logarithmic) and
#' returns it as a Wave object or vector.
#'
#' @param f0 Start frequency, in Hz
#' @param f1 End frequency, in Hz
#' @param mode One of "linear", "log"
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
#' w <- tuneR::stereo(sweptsine(100, 1e3))
#'
#' #Generate an exponentially swept sine wave between 100Hz and 1KHz.
#' w <- sweptsine(100, 1e3, mode="log")
#'
sweptsine <- function(f0=100, f1=2500, mode="linear", sweep.time=1, time.unit="seconds", samp.rate=44100, output="wave", ...) {
  .validateChoice(output, c("wave", "vector"), msg="output must be one of 'wave' or 'vector'")
  if (f1 <= f0) {
    stop("sweptsine: f1 must be greater than f0")
  }
  if (time.unit == "seconds") {
    vector_length <- samp.rate*sweep.time
  } else if (time.unit == "samples") {
    #A count of samples has to be a whole number. seq() silently rounded it, so
    #sweep.time=0.5 gave a sweep of one sample rather than an error.
    if (!isTRUE(round(sweep.time) == sweep.time)) {
      stop("sweptsine: with time.unit 'samples' sweep.time must be a whole number of samples")
    }
    vector_length <- sweep.time
  } else {
    stop("time.unit must be one of 'seconds' or 'samples'")
  }

  #The wave can only hold a whole number of samples, and seq() below rounds the
  #length to one. The duration is taken from the rounded length so that the
  #phase is integrated over the sweep that is actually generated: taken from the
  #unrounded length, it set the instantaneous frequency at the end of the sweep
  #to something the last sample never reached.
  vector_length <- round(vector_length)

  #Duration in seconds, however sweep.time was specified
  sweep.duration <- vector_length / samp.rate

  if (mode == "linear") {
    w <- sweptsine.lin(f0, f1, sweep.duration, samp.rate, vector_length)
  } else if (mode == "log") {
    if (f0 <= 0) {
      stop("sweptsine: f0 must be greater than zero in logarithmic mode")
    }
    w <- sweptsine.log(f0, f1, sweep.duration, samp.rate, vector_length)
  } else {
    stop("sweptsine: mode must be one of 'linear' or 'log'")
  }

  if (output == "vector") {
    return(w)
  }
  if (output == "wave") {
    return(data2Wave(w, samp.rate=samp.rate, remove.offset=FALSE, ...))
  }
}

sweptsine.lin <- function(f0, f1, sweep.time, samp.rate, vector_length) {
  #Phase is the integral of the instantaneous frequency f0 + (f1-f0)*t/sweep.time
  t <- seq(from=0, by=1/samp.rate, length=vector_length)
  phi <- 2 * pi * (f0 * t + (f1 - f0) * t^2 / (2 * sweep.time))
  x <- sin(phi)
  return(x)
}

sweptsine.log <- function(f0, f1, sweep.time, samp.rate, vector_length) {
  delta_t <- sweep.time / vector_length
  t <- seq(from=0, by=delta_t, length=vector_length)
  R <- log(f1/f0)
  x <- sin((2*pi*f0*sweep.time/R)*(exp(R*t/sweep.time)-1))
  return(x)
}
