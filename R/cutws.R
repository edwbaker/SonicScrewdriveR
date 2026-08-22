#' Cut wave by samples
#'
#' Extract a section of a Wave or WaveMC object based on sample positions. This
#' function will automatically detect if a Wave object is stereo, and returns
#' the same class of object it was given.
#'
#' @param wave A Wave or WaveMC object
#' @param from First sample to return
#' @param to Last sample to return
#' @param plot If TRUE shows the cut region within the original waveform
#' @return A Wave or WaveMC object
#' @export
#' @examples
#' \dontrun{
#' cutws(sheep, 1, 20)
#' cutws(sheep, 1, 20, plot=TRUE)
#' }
#'
cutws <- function(wave, from=1, to=Inf, plot=FALSE) {
  validateIsWaveLike(wave)
  if (is.infinite(to)) {
    to <- length(wave)
  }
  if (!is.numeric(from) | !is.numeric(to) | !(as.integer(from)==from) | !(as.integer(to) == to)) {
    stop("In cutws both from and to must be integers")
  }
  if (from > to){
    stop("In cutws to must be greater than from")
  }
  if (inherits(wave, "WaveMC")) {
    cutwave <- tuneR::WaveMC(wave@.Data[from:to, , drop=FALSE], samp.rate=wave@samp.rate, bit=wave@bit, pcm=wave@pcm)
  } else if (wave@stereo) {
    cutwave <- tuneR::Wave(wave@left[from:to], right=wave@right[from:to], samp.rate=wave@samp.rate, bit=wave@bit)
  } else {
    cutwave <- tuneR::Wave(wave@left[from:to], samp.rate=wave@samp.rate, bit=wave@bit)
  }

  if (plot) {
    seewave::oscillo(wave)
    graphics::abline(v=sDuration(c(from,to), wave=wave), col="red", lty=2)
  }
  return(cutwave)
}
