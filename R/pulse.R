pulse <- function(
  type="dirac",
  leading=22050,
  pulse.length=1,
  duration=samp.rate,
  samp.rate=44100,
  bit=1,
  pcm=FALSE,
  stereo=FALSE,
  output="Wave"
) {
  if (!type %in% c("dirac")) {
    stop("pulse type not recognised.")
  }
  if (leading > duration) {
    stop("leading cannot be greater than duration.")
  }
  pcm <- .setPCM(bit, pcm)
  w <- tuneR::silence(duration=duration, samp.rate=samp.rate, bit=bit, pcm=pcm, stereo=stereo)
  if (w@bit==8) {
    max <- 255
  } else {
    max <- 2^w@bit - 1
  }
  if (type=="dirac") {
    w@left[leading + 1] <- max
  }
  return(w)
}
