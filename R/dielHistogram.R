#' Diel Histogram
#'
#' Draws a histogram on a dielPlot() using pre-defined bins related to time of day.
#'
#' Each bin is closed at its start and open at its end, as the data frame
#' returned labels it, so a time falling exactly on a boundary is counted in the
#' bin that begins there: a recording made at 01:00 is in the 01:00 bin and not
#' in the midnight one.
#' @param times A vector of times that can be processed by dielFraction().
#' @param by Controls the size of histogram bins, one of "hour", "15minute", "30minute".
#' @param col Colour of the plot.
#' @param maxval By default scales histogram within limits, specifying a maximum value here allows comparison between plots.
#' @param presence.only Only show presence/absence not values.
#' @param limits Limits of the plotting (see dielPlot()).
#' @param rot Rotation of the plot, which must match the dielPlot() being drawn on.
#' @param timezone The timezone to read the times in, which decides which bin
#'   each falls in. The default, NULL, uses the timezone the times themselves
#'   carry, and UTC for times that carry none.
#' @return A data frame of start and end points of bins.
#' @export
#' @importFrom graphics hist
dielHistogram <- function(times, by="hour", col="grey", maxval=NA, presence.only=FALSE, limits=c(1,2), rot=tzRot(0), timezone=NULL) {
  #One bin per interval over the whole circle, so an hour is 24 bins of 2pi/24.
  minutes <- switch(by, hour = 60, `30minute` = 30, `15minute` = 15,
                    stop(paste("Unknown value of by for dielHistogram:", by)))
  breaks <- seq(from=0, to=2*pi, by=2*pi/(1440/minutes))

  angles <- cbind(breaks[-length(breaks)], breaks[-1])
  #A bin is closed at its start and open at its end, [from, to), which is how
  #the data frame returned labels it. hist() defaults to right=TRUE, so the bins
  #were (from, to] and a time recorded exactly on the hour - the normal case for
  #a scheduled recorder - was counted in the hour before it. Midnight was the
  #one exception, landing in its own bin only because include.lowest caught it,
  #so the convention was not even consistent with itself. include.lowest is left
  #at TRUE, which for right=FALSE closes the last bin of the day at 2*pi.
  h <- hist(dielFraction(times, timezone=timezone), breaks=breaks, plot=F,
            right=FALSE)

  if (is.na(maxval)) {
    maxval <- max(h$counts)
  }
  #No times, or a maximum of none, leaves nothing to scale against and nothing
  #to draw. Dividing by zero made every value NaN, and the test for an empty bin
  #below then failed with "missing value where TRUE/FALSE needed" rather than
  #giving back empty bins, which is what filtering a dataset down to no
  #detections in a window produces.
  if (maxval == 0) {
    maxval <- 1
  }

  data <- as.data.frame(cbind(angles, h$counts*diff(limits)/maxval))
  colnames(data) <- c("from", "to", "value")

  if (presence.only) {
    data$value[which(data$value > 0)] <- 1
  }

  for (i in seq_len(nrow(data))) {
    if (data$value[i]== 0) {
      next;
    }
    #Bars rise from the inner limit, and their heights are scaled by the width
    #of the limits above, so the fullest bar reaches the outer one. Drawn from a
    #literal radius of 1 they kept their scaled height but not their base, and
    #left the plot for any limits but c(1, 2): on a dielPlot() drawn with its
    #own default of c(0, 2) the fullest bar reached a radius of 3, outside the
    #region the plot draws at all.
    radialPolygon(data$from[i], data$to[i], limits[1], limits[1]+data$value[i],
                  col=col, rot=rot)
  }
  return(data)
}
