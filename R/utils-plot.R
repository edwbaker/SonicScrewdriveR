plotHMS.at <- function() {
  return(seq(0, 86400, by = 3600))
}

plotHMS.lab <- function() {
  return(sprintf("%02d00", 0:24))
}

#' Draw the legend for a polar plot
#'
#' dielPlot(), dielRings() and yearlyPlot() each drew the same legend, differing
#' only in where they put it and how large it is.
#'
#' @param labels Character vector of legend entries.
#' @param cols Colours matching `labels`.
#' @param x,y Position of the legend, in user coordinates.
#' @param cex Character expansion for the legend text.
#' @importFrom graphics legend
#' @return Called for its side effect of drawing a legend.
#' @noRd
.polarLegend <- function(labels, cols, x=-3, y=2.5, cex=1) {
  legend(x, y, labels, col=cols, lty=1, lwd=5, bty="n", cex=cex)
}
