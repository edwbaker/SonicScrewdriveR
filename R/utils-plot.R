#Conventions of the circular plots
#
#Every one of these plots turns a quantity into a position on a circle: a time
#into a position on a dielPlot(), a date into a position on a yearlyPlot(), a
#bearing into a position on a directionPlot(). Two choices decide where a value
#lands, and they are made the same way everywhere so that two rings drawn on one
#plot line up with each other and with the labels around them.
#
#A position is measured from the start of the period, in radians over
#[0, 2*pi): midnight for a day, 1 January for a year, north for a bearing. So
#counting begins at zero rather than at one - dielPositions("hours")[1] and
#yearlyPositions("days")[1] are both 0 - and the last position of a period is
#one step short of 2*pi rather than on top of the first. The length of the
#period comes from the value being placed, so a date in a leap year is measured
#against 366 days. Where a plot's own axis runs either side of its origin
#instead, as directionPlot()'s does over (-pi, pi], a position is wrapped into
#that range before it is drawn: it names the same place on the circle, and a
#position outside the range is dropped rather than drawn.
#
#A bin is closed at its start and open at its end, [from, to), so a value
#falling exactly on a boundary belongs to the bin that boundary opens: a
#recording made at 01:00 is counted in the 01:00 bin and not in the midnight
#one. The last bin of a period closes at 2*pi, so that the whole circle is
#covered.

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
