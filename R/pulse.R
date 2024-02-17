#' Generate a single pulse
#'
#' Generate a single pulse, either a Dirac pulse (Dirac delta)  or a square pulse.
#'
#' @param type Either "dirac" or "square".
#' @param leading The number of samples before the pulse.
#' @param pulse.length The number of samples in the pulse (for "square").
#' @param duration The total number of samples generated.
#' @param samp.rate The sample rate.
#' @param bit The bit depth.
#' @param pcm Whether Wave generated is PCM (see tuneR).
#' @param stereo Whether Wave generated is stereo.
#' @param output The output format ("Wave").
#' @param invert Whether to invert the pulse.
#' @return Specified by output.
#' @export
pulse <- function(
  type="dirac",
  leading=22050,
  pulse.length=1,
  duration=samp.rate,
  samp.rate=44100,
  bit=1,
  pcm=FALSE,
  stereo=FALSE,
  output="Wave",
  invert=FALSE
) {
  if (!type %in% c("dirac", "square")) {
    stop("pulse type not recognised.")
  }
  if (leading + pulse.length > duration) {
    stop("sum of leading and pulse.length cannot be greater than duration.")
  }
  if (!output %in% c("Wave", "TaggedWave")) {
    stop("output format not recognised.")
  }
  pcm <- .setPCM(bit, pcm)
  w <- tuneR::silence(duration=duration, samp.rate=samp.rate, bit=bit, pcm=pcm, stereo=stereo)
  if (w@bit==8) {
    if (invert) {
      max <- 0
    } else {
      max <- 255
    }
  } else {
    if (pcm) {
      if (invert) {
        max <- -2^w@bit / 2
      } else {
        max <- 2^w@bit / 2
      }
    } else {
      if (invert) {
        max <- -2^w@bit
      } else {
        max <- 2^w@bit
      }
    }
  }
  if (type=="dirac") {
    w@left[leading + 1] <- max
    if (stereo) {
      w@right[leading + 1] <- max
    }
  }
  if (type=="square") {
    w@left[(leading + 1):(leading + pulse.length)] <- max
    if (stereo) {
      w@right[(leading + 1):(leading + pulse.length)] <- max
    }
  }
  if (output=="Wave") {
    return(w)
  }
  if (output=="TaggedWave") {
    return(tagWave(w, paste("pulse", type, sep="-")))
  }
}
