#' Beat spectrum complexity
#'
#' This function computes a \code{beatSpectrum} and calculates some basic
#' measurements of its complexity. The complexity value is calculated as the
#' maximum identified repeating period (in seconds) divided by the number of
#' peaks.
#'
#' @param wave A Wave object
#' @param plot If TRUE a spectrogram overlaid with the peaks is plotted.
#' @return A list of the complexity, a vector of the peak periods, and the number of peaks.
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

  r <- list(
    "complexity" = max(bs$period[peaks]) / length(bs$period[peaks]),
    "peak.periods" = bs$period[peaks],
    "num.peaks" = length(bs$period[peaks])
  )
  return(r)

}
