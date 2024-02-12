#' Generate labels for a diel plot
#'
#' Generates labels for a dielPlot() in 12- or 24-hour format. Labels are generated
#' at three hourly intervals.
#'
#' @param format One of clock24 (default) or clock12
#' @export
#' @examples
#' dielLabels()
#' dielLabels("clock12")
dielLabels <- function(format="clock24") {
  if (!format %in% c("clock24", "clock12")) {
    stop(paste("Unknown format for dielLabels:",format))
  }
  if (format=="clock24") {
    return(c("0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100"))
  }
  if (format=="clock12") {
    return(c("0000", "0300 AM", "0600 AM", "0900 AM", "1200 NOON", "0300 PM", "0600 PM", "0900 PM"))
  }
}

#' Generate positions of labels for a diel plot
#'
#' Generates positions for three-hourly labels of a dielPlot() in radians.
#' @param format One of "3hours" (default), "hours", or "minutes"
#' @export
#' @examples
#' dielPositions()
#' dielPositions("hours")
#' dielPositions("minutes")
dielPositions <- function(format="3hourly") {
  if (!format %in% c("3hourly", "hours", "minutes")) {
    stop(paste("Unknown format for dielPositions:",format))
  }
  if (format == "3hourly") {
    return(2*pi * c(0, 45, 90, 135, 180, 225, 270, 315)/360)
  }
  if (format == "hours") {
    return(2*pi * (1:24)/24)
  }
  if (format == "minutes") {
    mpd <- 24*60
    return(2*pi * (1:mpd)/mpd)
  }
}

#' Calculate the fraction of a day given by a value
#'
#' Given an object that can be coerced to POSIXlt or is in a supported string
#' format, return the fraction of a day represented by the object.
#'
#' @param t Object to be converted to a fraction
#' @param input One of POSIX (default) or HHMM
#' @param unit If set to radians outputs a position around a circle. If set to fraction outputs the raw fraction.
#' @export
dielFraction <- function(t, input="POSIX", unit="radians") {
  if (!input %in% .convertable2seconds()) {
    stop(paste("Unknown input for dielFraction:",input))
  }
  if (!unit %in% c("radians", "fraction")) {
    stop(paste("Unknown output for dielFraction:",unit))
  }
  s <- convert2seconds(t, input=input, origin="day")
  f <- s/(24*60*60)
  if (unit=="radians") {
    return(2*pi*f)
  }
  return(f)
}

.dielPlotMethods <- function() {
  return(c("plotrix"))
}

#' Create an empty diel plot
#'
#' Create a diel plot with labels but without sun altitude or times of day plotted.
#' @param method Plotting package to use
#' @param rot Rotation of the origin (defaults to pi)
#' @export
emptyDiel <- function(method="plotrix", rot=pi) {
  if (!method %in% .dielPlotMethods()) {
    stop(paste("Unknown method for emptyDiel:",method))
  }
  if (method == "plotrix") {
    plotrix::radial.plot(
      lengths=0,
      radial.pos=0,
      rp.type="p",
      radial.lim=c(0,1,2),
      start=rot,
      label.pos = dielPositions(),
      labels=dielLabels(),
      clockwise=T,
      poly.col=rgb(1,1,0, 0.6),
      lty=0,
      show.grid.labels = F)
  }
}

#' Converts a timezone offset into a rotation
#'
#' Given a timezone offset in hours returns a rotation in radians to apply to values for a diel plot.
#' @param tz Timezone numeric
#' @param init Initial rotation. Defaults to pi.
#' @export
tzRot <- function(tz, init=pi) {
  return(init + -tz*2*pi/24)
}

#' Create a diel plot
#'
#' A diel plot shows the times of night, twilight and the maximum altitude of the sun for a given date.
#'
#' @param date Date to plot.
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @param plot Character vector of components to plot
#' @param rot Either "Solar Noon" or an offset calculated by tz
#' @param limits Plotting limits of the daylight regions, default to c(1,2)
#' @param method Plotting library to use
#' @param legend Whether to show a legend
#' @export
#' @importFrom suncalc getSunlightPosition getSunlightTimes
dielPlot <- function(
    date,
    lat,
    lon,
    limits=c(0,2),
    plot=NULL,
    rot=tzRot(0),
    method="plotrix",
    legend=F
){
  date <- as.POSIXlt(date)
  times <- seq.POSIXt(from=date, by="min", length.out=60*24)
  attr(times, 'tzone') <- "UTC"
  #Calculate night time from sun altitude above horizon
  pos <- getSunlightPosition(date = times, lat = lat, lon = lon, keep = c("altitude"))
  tim <- getSunlightTimes(date = as.Date(times[1]), lat = lat, lon = lon)
  if (rot=="Solar Noon") {
    df <- dielFraction(tim$solarNoon)
    rot <- pi-(df-pi)
  }
  day <- pos$altitude*2/pi
  day[which(day < 0)] <- 0

  if (method=="plotrix") {
    #Scale for limits
    day <- day * (limits[2]-limits[1])
    if (!package.installed("plotrix")){stop("Plotrix must be installed to plot using Plotrix.")}
    emptyDiel(rot=rot)

    angles <- dielFraction(pos$date)
    radialPolygon(NA,angles, limits[1], limits[1]+day,col=rgb(1,1,0, 0.6), rot=rot)

    alt <- getSunlightPosition(tim$solarNoon, lat=tim$lat, lon=tim$lon, keep=c("altitude"))$altitude
    leg <- c()
    col <- c()

    if (is.null(plot) | "Civil Twilight" %in% plot) {
      leg <- c(leg, "Civil Twilight")
      col <- c(col, rgb(0.8,0.8,0.8,1))
      if (!is.na(tim$sunrise)) {
        radialPolygon(dielFraction(tim$sunset), dielFraction(tim$sunrise), limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), rot=rot)
      }
      if (is.na(tim$sunrise) & !is.na(tim$dawn)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), rot=rot)
      }
    }
    if (is.null(plot) | "Nautical Twilight" %in% plot) {
      leg <- c(leg, "Nautical Twilight")
      col <- c(col, rgb(0.6,0.6,0.6,1))
      if (!is.na(tim$dawn)) {
        radialPolygon(dielFraction(tim$dusk), dielFraction(tim$dawn),limits[1], limits[2], col=rgb(0.6,0.6,0.6,1), rot=rot)
      }
      if (is.na(tim$dawn) & !is.na(tim$nauticalDawn)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), rot=rot)
      }
    }
    if (is.null(plot) |"Astronomical Twilight" %in% plot) {
      leg <- c(leg, "Astronomical Twilight")
      col <- c(col, rgb(0.4,0.4,0.4,1))
      if (!is.na(tim$nauticalDawn)) {
        radialPolygon(dielFraction(tim$nauticalDusk), dielFraction(tim$nauticalDawn), limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), rot=rot)
      }
      if (is.na(tim$nauticalDawn) & !is.na(tim$night)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), rot=rot)
      }
    }
    if (is.null(plot) |"Night" %in% plot) {
      leg <- c(leg, "Night")
      col <- c(col, rgb(0.2,0.2,0.2,1))
      if (!is.na(tim$night) & !is.na(tim$nightEnd)){
        radialPolygon(dielFraction(tim$night), dielFraction(tim$nightEnd),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), rot=rot)
      }
      if(alt <= -0.314159) {
        if (is.na(tim$night)) {
          radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), rot=rot)
        }
      }
    }
    if ("Nadir" %in% plot) {
      radialPolygon(dielFraction(tim$nadir), dielFraction(tim$nadir),limits[1],limits[2], col=rgb(0,0,0,1))
    }

    if (is.null(plot) | "Sunrise" %in% plot) {
      if (!is.na(tim$sunrise)) {
        if (is.na(tim$sunriseEnd)) {
          tim$sunriseEnd <- tim$solarNoon
        }
        radialPolygon(dielFraction(tim$sunrise),dielFraction(tim$sunriseEnd),limits[1],limits[2], col=rgb(1,0.5,0,1), rot=rot)
      }
    }
    if (is.null(plot) |"Sunset" %in% plot) {
      if (!is.na(tim$sunset)) {
        if (is.na(tim$sunsetStart)) {
          tim$sunsetStart <- tim$solarNoon
        }
        radialPolygon(dielFraction(tim$sunsetStart),dielFraction(tim$sunset),limits[1],limits[2], col=rgb(1,0.5,0,1), rot=rot)
      }
    }
    if ("Solar Noon" %in% plot) {
      radialPolygon(dielFraction(tim$solarNoon), dielFraction(tim$solarNoon),limits[1],limits[2], col=rgb(1,0.5,0,1), rot=rot)
    }

    if (legend) {
      legend(
        -3,2.5,
        leg,
        col=col,
        lty=1,
        lwd=5,
        bty = "n",
        cex = 1)
    }
  }
}

#' Plot rings on a diel plot
#'
#' Plot rings on a diel plot.
#' @param names Labels for the rings
#' @param starts Start times for rings in HHMM string format
#' @param ends End times for rings in HHMM string format
#' @param cols Colours of the rings
#' @param format Defaults to HHMM
#' @param limits Region of a dielPlot() to plot rings. Defaults to c(1,2)
#' @param legend Boolean. Whether to plot a legend.
#' @export
dielRings <- function(names, starts, ends, cols = "grey", format="HHMM", limits=c(1,2), legend=T) {
  cols <- rep_len(cols, length.out = length(names))

  #Convert to fractional circle
  starts <- dielFraction(starts, input=format)
  ends <- dielFraction(ends, input=format)

  arc_step <- (limits[2] - limits[1]) / length(names)
  arcs <- limits[1] + arc_step * (1:length(names)-1)

  for (i in 1:length(names)) {
    radialPolygon(starts[i],ends[i], arcs[i],arcs[i]+0.1, col=cols[i])
  }

  if (legend) {
    legend(
      -3.5,-1.75,
      names,
      col=cols,
      lty=1,
      lwd=5,
      bty = "n",
      cex = 0.75)
  }
}
