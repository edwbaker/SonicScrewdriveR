#' Plot a radial polygon
#'
#' Used to plot sectors, annuli and horizons on a dielPlot() or yearlyPlot(). The polygon
#' has an inner and outer horizon - which can be set to a fixed radius or a vector.
#' @param angle1 Angles for the inner line
#' @param angle2 Angles for the outer line
#' @param radius1 Radii for the inner line
#' @param radius2 Radii for the outer line
#' @param col Colour of the polygon
#' @param border Border colour (see polygon() for details)
#' @param rot Rotation of the plot, defaults to pi to match dielPlot() and yearlyPlot()
#' @param angleinc The angular increment in radians for calculating circular lines
#' @param reverse If FALSE plots in an anti-clockwise direction
#' @param ... Other parameters passed to polygon()
#' @importFrom graphics polygon
#' @export
radialPolygon <- function(
    angle1,angle2,
    radius1,radius2,
    col="grey",
    border=NA,
    rot=-pi,
    angleinc=0.01,
    reverse=TRUE,
    ...
) {

  if (length(angle1) == 1 & length(angle2) == 1) {
    while (angle1 > angle2) {
      angle2 <- angle2 + 2*pi
    }
  }
  if (reverse) {
    angle1 <- -angle1
    angle2 <- -angle2
  }
  min_a <- min(c(angle1, angle2), na.rm=T)
  max_a <- max(c(angle1, angle2), na.rm=T)

  angles<-seq(min_a,max_a,by=angleinc)
  angles[length(angles)]<-max_a
  if (min_a < 0) {
    angles <- rev(angles)
  }

  if (length(radius1) == 1 & length(radius2) == 1) {
    inner_x <- cos(angles)*radius1
    inner_y <- sin(angles)*radius1
    outer_x <- cos(angles)*radius2
    outer_y <- sin(angles)*radius2
  } else {
    if (length(radius1) == 1) {
      if (radius1 == 0) {
        inner_x <- 0
        inner_y <- 0
      } else {
        inner_x <- cos(angles)*radius1
        inner_y <- sin(angles)*radius1
      }
    } else {
      inner_x <- radius1*cos(angle1)
      inner_y <- radius1*sin(angle1)
    }
    if (length(radius2) == 1) {
      outer_x <- cos(angles)*radius2
      outer_y <- sin(angles)*radius2
    } else {
      outer_x <- radius2*cos(angle2)
      outer_y <- radius2*sin(angle2)
    }
  }

  outer_x <- rev(outer_x)
  outer_y <- rev(outer_y)
  xpos<-c(inner_x,outer_x)
  ypos<-c(inner_y,outer_y)

  if (rot != 0) {
    xrot <- xpos*cos(rot) + ypos*sin(rot)
    yrot <- -xpos*sin(rot) + ypos*cos(rot)
    xpos <- xrot
    ypos <- yrot
  }

  polygon(xpos,ypos,col=col,border=border,...)
}

#' Circularise a dataset
#'
#' When plotting rings or horizons that are meant to cover the entirety of the time period in a
#' dielPlot() or yearlyPlot() this function append the beginning values to the end to ensure an entire
#' loop is created.
#' @param values A vector if values
#' @export
circularise <- function(values) {
  return(c(values, values[1]))
}

#' Diel Histogram
#'
#' Draws a histogram on a dielPlot() using pre-defined bins related to time of day.
#' @param times A vector of times that can be pocessed by dielFraction().
#' @param by Controls the size of histogram bins, one of "hour", "15minute", "30minute"
#' @param col Colour of the plot
#' @param maxval By default scales histogram within limits, specifying a maximum value here allows comparison between plots
#' @param limits Limits of the plotting (see dielPlot())
#' @return A data frame of start and end points of bins.
#' @export
#' @importFrom graphics hist
dielHistogram <- function(times, by="hour", col="grey", maxval=NA, limits=c(1,2)) {
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

    for (i in 1:nrow(data)) {
    if (data$value[i]== 0) {
      next;
    }
    radialPolygon(data$from[i],data$to[i], 1,1+data$value[i], col=col)
  }
  return(data)
}
