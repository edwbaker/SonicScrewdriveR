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
  .validateChoice(type, c("dirac", "square"), msg="pulse type not recognised.")
  if (leading + pulse.length > duration) {
    stop("sum of leading and pulse.length cannot be greater than duration.")
  }
  .validateChoice(output, c("Wave", "TaggedWave"), msg="output format not recognised.")
  pcm <- .setPCM(bit, pcm)
  w <- tuneR::silence(duration=duration, samp.rate=samp.rate, bit=bit, pcm=pcm, stereo=stereo)
  #The amplitude has to suit the unit that was asked for. tuneR::silence() resolves
  #bit=1 to a Wave whose bit slot reads 32, so taking the amplitude from that slot
  #put every pulse that was not 8-bit far outside the range its format allows and
  #writeWave() refused to write the result.
  if (invert) {
    #Eight bit PCM is unsigned, so its quietest value is zero rather than the
    #negative of its loudest.
    max <- if (w@bit == 8) 0 else -.waveFullScale(w)
  } else {
    max <- .waveFullScale(w)
  }
  if (type=="dirac") {
    w@left[leading + 1] <- max
    if (stereo) {
      w@right[leading + 1] <- max
    }
  }
  if (type=="square") {
    #seq_len() so that a pulse of no length writes nothing, where (leading+1):
    #(leading+0) counted backwards and wrote two samples.
    samples <- leading + seq_len(pulse.length)
    w@left[samples] <- max
    if (stereo) {
      w@right[samples] <- max
    }
  }
  if (output=="Wave") {
    return(w)
  }
  if (output=="TaggedWave") {
    return(tagWave(w, paste("pulse", type, sep="-")))
  }
}
