#' Cut wave by samples
#'
#' Extract a section of a Wave object based on sample positions. This function
#' will automatically detect if a Wave object is stereo.
#'
#' @param wave A Wave object
#' @param from First sample to return
#' @param to Last sample to return
#' @param plot If TRUE shows the cut region within the original waveform
#' @return A Wave object
#' @export
#' @examples
#' \dontrun{
#' cutws(sheep, 1, 20)
#' cutws(sheep, 1, 20, plot=TRUE)
#' }
#'
cutws <- function(wave, from=1, to=Inf, plot=FALSE) {
  validateIsWave(wave)
  if (is.infinite(to)) {
    to <- length(wave)
  }
  if (!is.numeric(from) | !is.numeric(to) | !(as.integer(from)==from) | !(as.integer(to) == to)) {
    stop("In cutws both from and to must be integers")
  }
  if (from > to){
    stop("In cutws to must be greater than from")
  }
  if (wave@stereo) {
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
