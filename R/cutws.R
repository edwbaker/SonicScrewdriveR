#' Cut wave by samples
#'
#' Extract a section of a Wave or WaveMC object based on sample positions. This
#' function will automatically detect if a Wave object is stereo, and returns
#' the same class of object it was given.
#'
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()]. The samples
#' that were cut to are recorded, rather than the ones that were asked for,
#' which may have run beyond the end of the wave.
#'
#' @param wave A Wave or WaveMC object
#' @param from First sample to return
#' @param to Last sample to return. A value beyond the end of the wave is cut
#'   to the final sample.
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
  if (to > length(wave)) {
    to <- length(wave)
  }
  if (from > to){
    stop("In cutws to must be greater than from")
  }
  return(.tagApplyOne(wave, .cutws, "cutws", NULL, TRUE, from=from, to=to, plot=plot))
}

#' Cut a wave by samples without recording it
#'
#' An untagged wave is cut, as `plot=TRUE` draws with seewave, which rejects the
#' tagged classes.
#' @keywords internal
#' @noRd
.cutws <- function(wave, from, to, plot) {
  if (inherits(wave, "WaveMC")) {
    cutwave <- tuneR::WaveMC(wave@.Data[from:to, , drop=FALSE], samp.rate=wave@samp.rate, bit=wave@bit, pcm=wave@pcm)
  } else if (wave@stereo) {
    cutwave <- tuneR::Wave(wave@left[from:to], right=wave@right[from:to], samp.rate=wave@samp.rate, bit=wave@bit, pcm=wave@pcm)
  } else {
    cutwave <- tuneR::Wave(wave@left[from:to], samp.rate=wave@samp.rate, bit=wave@bit, pcm=wave@pcm)
  }

  if (plot) {
    seewave::oscillo(wave)
    graphics::abline(v=sDuration(c(from,to), wave=wave), col="red", lty=2)
  }
  return(cutwave)
}
