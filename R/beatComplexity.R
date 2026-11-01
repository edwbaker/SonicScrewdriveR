#' Beat spectrum complexity
#'
#' This function computes a \code{beatSpectrum} and calculates some basic
#' measurements of its complexity. The complexity value is calculated as the
#' maximum identified repeating period (in seconds) divided by the number of
#' peaks.
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which measures each channel separately.
#' @param plot If TRUE a spectrogram overlaid with the peaks is plotted. One
#'   plot is drawn per channel, so a multi-channel wave needs a layout of its
#'   own for all of them to be seen.
#' @return A list of the complexity, a vector of the peak periods, and the number of
#'   peaks. The complexity is NA if the beat spectrum has no peak. For
#'   multi-channel input, one such list per channel.
#' @examples
#' \dontrun{
#'   beatComplexity(sheep)
#'   beatComplexity(sheep, plot=TRUE)
#' }
#' @export
#'

beatComplexity <-function(
  wave,
  plot=FALSE
) {
  #The complexity of a stereo recording was that of its left channel alone, as
  #beatSpectrum() read only the left slot, and a WaveMC has no such slot.
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(wave, function(w) beatComplexity(w, plot=plot), channel.param=NULL))
  }
  bs <- beatSpectrum(wave)

  e <- c(0,diff(sign(diff(bs$power))),0)
  peaks <- e==-2

  if (plot) {
    plot(bs$period, bs$power, type="l")
    abline(v=bs$period[peaks], col="green")
  }

  #A spectrum with no peak has no longest repeating period. max() of nothing
  #warns and returns -Inf, which was then divided by a peak count of zero and
  #returned as the complexity. The complexity is undefined in that case, not
  #infinitely negative.
  periods <- bs$period[peaks]
  r <- list(
    "complexity" = if (length(periods) == 0) NA_real_ else max(periods) / length(periods),
    "peak.periods" = periods,
    "num.peaks" = length(periods)
  )
  return(r)

}
