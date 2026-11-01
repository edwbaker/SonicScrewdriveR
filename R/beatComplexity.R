#' Beat spectrum complexity
#'
#' This function computes a \code{beatSpectrum} and calculates some basic
#' measurements of its complexity. The complexity value is calculated as the
#' maximum identified repeating period (in seconds) divided by the number of
#' peaks.
#'
#' @param wave A Wave object
#' @param plot If TRUE a spectrogram overlaid with the peaks is plotted.
#' @return A list of the complexity, a vector of the peak periods, and the number of
#'   peaks. The complexity is NA if the beat spectrum has no peak.
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
