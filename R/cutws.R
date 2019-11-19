#' Cut wave by samples
#'
#' Extract a section of a Wave object based on sample positions
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
cutws <- function(wave, from, to, plot=FALSE) {
  if (typeof(wave) != "S4" | class(wave) != "Wave") {
    stop("cutws expects a Wave object")
  }
  if (!is.numeric(from) | !is.numeric(to) | !(as.integer(from)==from) | !(as.integer(to) == to)) {
    stop("In cutws both from and to must be integers")
  }
  if (from > to){
    stop("In cutws to must be greater than from")
  }
  cutwave <- tuneR::Wave(wave@left[from:to], samp.rate=wave@samp.rate, bit=wave@bit)
  if (plot) {
    seewave::oscillo(wave)
    graphics::abline(v=sDuration(c(from,to), wave=wave), col="red", lty=2)
  }
  return(cutwave)
}
