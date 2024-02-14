#' Diel Histogram
#'
#' Draws a histogram on a dielPlot() using pre-defined bins related to time of day.
#' @param times A vector of times that can be pocessed by dielFraction().
#' @param by Controls the size of histogram bins, one of "hour", "15minute", "30minute".
#' @param col Colour of the plot.
#' @param maxval By default scales histogram within limits, specifying a maximum value here allows comparison between plots.
#' @param presence.only Only show presence/absence not values.
#' @param limits Limits of the plotting (see dielPlot()).
#' @return A data frame of start and end points of bins.
#' @export
#' @importFrom graphics hist
dielHistogram <- function(times, by="hour", col="grey", maxval=NA, presence.only=FALSE, limits=c(1,2)) {
  if (by=="hour") {
    breaks <- seq(from=0,to=2*pi,by=(pi/12))
  } else if (by=="15minute") {
    breaks <- seq(from=0,to=2*pi,by=(pi/120))
  } else if (by=="30minute") {
    breaks <- seq(from=0,to=2*pi,by=(pi/60))
  }

  angles <- cbind(breaks[-length(breaks)], breaks[-1])
  h <- hist(dielFraction(times), breaks=breaks, plot=F)

  if (is.na(maxval)) {
    maxval <- max(h$counts)
  }

  data <- as.data.frame(cbind(angles, h$counts*diff(limits)/maxval))
  colnames(data) <- c("from", "to", "value")

  if (presence.only) {
    data$value[which(data$value > 0)] <- 1
  }

  for (i in 1:nrow(data)) {
    if (data$value[i]== 0) {
      next;
    }
    radialPolygon(data$from[i],data$to[i], 1,1+data$value[i], col=col)
  }
  return(data)
}
