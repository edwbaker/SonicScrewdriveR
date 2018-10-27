cutws <- function(wave, from, to, plot=FALSE) {
  cutwave <- tuneR::Wave(wave@left[from:to], samp.rate=wave@samp.rate, bit=wave@bit)
  if (plot) {
    seewave::oscillo(wave)
    graphics::abline(v=sDuration(c(from,to), wave=wave), col="red", lty=2)
  }
  return(cutwave)
}