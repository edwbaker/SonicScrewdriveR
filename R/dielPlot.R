#' Generate labels for a diel plot
#'
#' Generates labels for a dielPlot() in 12- or 24-hour format. Labels are generated
#' at three hourly intervals.
#'
#' @param format One of clock24 (default) or clock12
#' @return A character vector of eight labels, at three hourly intervals.
#' @export
#' @examples
#' dielLabels()
#' dielLabels("clock12")
dielLabels <- function(format="clock24") {
  .validateChoice(format, c("clock24", "clock12"), "format", "dielLabels", prep="for")
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
#' @param format One of "3hourly" (default), "hours", or "minutes"
#' @return A numeric vector of label positions, in radians.
#' @export
#' @examples
#' dielPositions()
#' dielPositions("hours")
#' dielPositions("minutes")
dielPositions <- function(format="3hourly") {
  .validateChoice(format, c("3hourly", "hours", "minutes"), "format", "dielPositions", prep="for")
  if (format == "3hourly") {
    return(2*pi * c(0, 45, 90, 135, 180, 225, 270, 315)/360)
  }
  #Counted from zero, so that there is a position at midnight. Counting from one
  #left midnight out and repeated it at the end as 2*pi.
  if (format == "hours") {
    return(2*pi * (0:23)/24)
  }
  if (format == "minutes") {
    mpd <- 24*60
    return(2*pi * (0:(mpd-1))/mpd)
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
#' @return The position of the time within the day, in radians or as a fraction of a day.
#' @export
dielFraction <- function(t, input="POSIX", unit="radians") {
  .validateChoice(input, .convertable2seconds(), "input", "dielFraction", prep="for")
  .validateChoice(unit, c("radians", "fraction"), "output", "dielFraction", prep="for")
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
#' @return Called for its side effect of drawing a plot. The return value is that of the underlying plotting function and should not be relied on.
#' @export
emptyDiel <- function(method="plotrix", rot=pi) {
  .validateChoice(method, .dielPlotMethods(), "method", "emptyDiel", prep="for")
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
#' @return The rotation of the plot, in radians.
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
#' @return Called for its side effect of drawing a plot. The return value is that of the underlying plotting function and should not be relied on.
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
    legend=FALSE
){
  #Built in UTC from the start. Taking local midnight and then relabelling the
  #instants as UTC moved them by the session's offset, so a session east of UTC
  #drew the previous day's twilight.
  date <- as.Date(date)
  times <- seq(
    from = as.POSIXct(paste(date, "00:00:00"), tz="UTC"),
    by = "min",
    length.out = 60*24
  )
  #Calculate night time from sun altitude above horizon
  pos <- getSunlightPosition(date = times, lat = lat, lon = lon, keep = c("altitude"))
  tim <- getSunlightTimes(date = date, lat = lat, lon = lon, tz = "UTC")
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

    #The three outer twilight bands differ only in their name, their grey, and
    #which three sun events they read, so they are driven from a table. Night is
    #written out below because it is not the same shape: it needs both of its
    #events, and falls back on the sun's altitude rather than on a later event.
    bands <- list(
      list(name="Civil Twilight",         grey=0.8, start="sunset",       end="sunrise",      next.event="dawn"),
      list(name="Nautical Twilight",      grey=0.6, start="dusk",         end="dawn",         next.event="nauticalDawn"),
      list(name="Astronomical Twilight",  grey=0.4, start="nauticalDusk", end="nauticalDawn", next.event="night")
    )
    for (band in bands) {
      if (!is.null(plot) & !band$name %in% plot) {
        next
      }
      band.col <- rgb(band$grey, band$grey, band$grey, 1)
      leg <- c(leg, band$name)
      col <- c(col, band.col)
      if (!is.na(tim[[band$end]])) {
        radialPolygon(
          dielFraction(tim[[band$start]]), dielFraction(tim[[band$end]]),
          limits[1], limits[2], col=band.col, rot=rot
        )
      }
      if (is.na(tim[[band$end]]) & !is.na(tim[[band$next.event]])) {
        radialPolygon(0, 2*pi, limits[1], limits[2], col=band.col, rot=rot)
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
      #rot was missing here alone, so the nadir marker was drawn unrotated while
      #every other band on the plot was rotated.
      radialPolygon(dielFraction(tim$nadir), dielFraction(tim$nadir),limits[1],limits[2], col=rgb(0,0,0,1), rot=rot)
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
      .polarLegend(leg, col)
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
#' @param rot Rotation of the plot, which must match the dielPlot() being drawn on.
#' @return Called for its side effect of drawing on the current plot. The return value is that of the underlying plotting function and should not be relied on.
#' @export
dielRings <- function(names, starts, ends, cols = "grey", format="HHMM", limits=c(1,2), legend=TRUE, rot=tzRot(0)) {
  cols <- rep_len(cols, length.out = length(names))

  #Convert to fractional circle
  starts <- dielFraction(starts, input=format)
  ends <- dielFraction(ends, input=format)

  if (length(names) == 0) {
    return(invisible(NULL))
  }

  arc_step <- (limits[2] - limits[1]) / length(names)
  arcs <- limits[1] + arc_step * (seq_along(names) - 1)

  #Each ring is as thick as the space allotted to it, so that the rings fill the
  #region between the limits however many there are. A fixed thickness meant that
  #more than ten rings overlapped one another and ran past limits[2].
  for (i in seq_along(names)) {
    radialPolygon(starts[i], ends[i], arcs[i], arcs[i]+arc_step, col=cols[i], rot=rot)
  }

  if (legend) {
    .polarLegend(names, cols, x=-3.5, y=-1.75, cex=0.75)
  }
}
