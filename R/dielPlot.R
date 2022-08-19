#' @export
dielLabels <- function(format="clock24", pos=NULL) {
  if (is.null(pos)) {
    if (format=="clock24") {
      ret <- c("0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100")
    }
    if (format=="clock12") {
      ret <- c("0000", "0300 AM", "0600 AM", "0900 AM", "1200 NOON", "0300 PM", "0600 PM", "0900 PM")
    }
  }
  else {
    ret <- c(0, 45, 90, 135, 180, 225, 270, 315)
  }
  return(ret)
}

#' @export
dailyFraction <- function(t, unit="radians") {
  t <- unclass(as.POSIXlt(t))
  f <- (t$sec + 60*t$min + 3600*t$hour)/86400
  if (unit=="radians") {
    return(2*pi*f)
  }
  return(f)
}

#'@export
emptyDiel <- function(method="plotrix") {
  if (method == "plotrix") {
    plotrix::radial.plot(
      lengths=0,
      radial.pos=0,
      rp.type="p",
      radial.lim=c(0,1,2),
      start=pi,
      label.pos = dielLabels(pos='pos')*pi/180,
      labels=dielLabels(),
      clockwise=T,
      poly.col=rgb(1,1,0, 0.6),
      lty=0,
      show.grid.labels = F)
  }
}

#' Create a diel plot
#'
#' A diel plot shows the times of night, twilight and the maximum altitude of the sun for a given date.
#'
#' @param date Date to plot.
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @param plot Character vector of components to plot
#' @param limits Plotting limits of the daylight regions, default to c(1,2)
#' @param method Plotting library to use
#' @param legend Whether to show a legend
#' @export
#' @importFrom suncalc getSunlightPosition getSunlightTimes
dielPlot <- function(date, lat, lon, limits=c(0,2), plot=NULL, method="plotrix", legend=F) {
  date <- as.POSIXlt(date)
  times <- seq.POSIXt(from=date, by="min", length.out=60*24)
  attr(times, 'tzone') <- "UTC"
  #Calculate night time from sun altitude above horizon
  pos <- getSunlightPosition(date = times, lat = lat, lon = lon, keep = c("altitude"))
  tim <- getSunlightTimes(date = as.Date(times[1]), lat = lat, lon = lon)
  day <- pos$altitude*2/pi
  day[which(day < 0)] <- 0


  if (method=="plotrix") {
    #Scale for limits
    day <- day * (limits[2]-limits[1])
    if (!package.installed("plotrix")){stop("Plotrix must be installed to plot using Plotrix.")}
    if (limits[1] == 0) {
      plotrix::radial.plot(
        lengths=day,
        radial.pos=2*pi*seq_along(day)/length(day),
        rp.type="p",
        radial.lim=c(0,1,2),
        start=pi,
        label.pos = dielLabels(pos='pos')*pi/180,
        labels=dielLabels(),
        clockwise=T,
        poly.col=rgb(1,1,0, 0.6),
        lty=0,
        show.grid.labels =F
      )
    } else {
      emptyDiel()

      angles <- 2*pi*seq_along(day)/length(day)
      radialPolygon(NA,angles, limits[1], limits[1]+day,col=rgb(1,1,0, 0.6))
    }

    alt <- getSunlightPosition(tim$solarNoon, lat=tim$lat, lon=tim$lon, keep=c("altitude"))$altitude

    leg <- c()
    col <- c()

    if (is.null(plot) | "Civil Twilight" %in% plot) {
      leg <- c(leg, "Civil Twilight")
      col <- c(col, rgb(0.8,0.8,0.8,1))
      if (!is.na(tim$sunrise)) {
        radialPolygon(dailyFraction(tim$sunset), dailyFraction(tim$sunrise), limits[1],limits[2], col=rgb(0.8,0.8,0.8,1))
      }
      if (is.na(tim$sunrise) & !is.na(tim$dawn)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.8,0.8,0.8,1))
      }
    }
    if (is.null(plot) | "Nautical Twilight" %in% plot) {
      leg <- c(leg, "Nautical Twilight")
      col <- c(col, rgb(0.6,0.6,0.6,1))
      if (!is.na(tim$dawn)) {
        radialPolygon(dailyFraction(tim$dusk), dailyFraction(tim$dawn),limits[1], limits[2], col=rgb(0.6,0.6,0.6,1))
      }
      if (is.na(tim$dawn) & !is.na(tim$nauticalDawn)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.6,0.6,0.6,1))
      }
    }
    if (is.null(plot) |"Astronomical Twilight" %in% plot) {
      leg <- c(leg, "Astronomical Twilight")
      col <- c(col, rgb(0.4,0.4,0.4,1))
      if (!is.na(tim$nauticalDawn)) {
        radialPolygon(dailyFraction(tim$nauticalDusk), dailyFraction(tim$nauticalDawn), limits[1],limits[2], col=rgb(0.4,0.4,0.4,1))
      }
      if (is.na(tim$nauticalDawn) & !is.na(tim$night)) {
        radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.4,0.4,0.4,1))
      }
    }
    if (is.null(plot) |"Night" %in% plot) {
      leg <- c(leg, "Night")
      col <- c(col, rgb(0.2,0.2,0.2,1))
      if (!is.na(tim$night) & !is.na(tim$nightEnd)){
        radialPolygon(dailyFraction(tim$night), dailyFraction(tim$nightEnd),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1))
      }
      if(alt <= -0.314159) {
        if (is.na(tim$night)) {
          radialPolygon(0, 2*pi,limits[1],limits[2], col=rgb(0.2,0.2,0.2,1))
        }
      }
    }
    if ("Nadir" %in% plot) {
      radialPolygon(dailyFraction(tim$nadir), dailyFraction(tim$nadir),limits[1],limits[2], col=rgb(0,0,0,1))
    }

    if (is.null(plot) | "Sunrise" %in% plot) {
      if (!is.na(tim$sunrise)) {
        if (is.na(tim$sunriseEnd)) {
          tim$sunriseEnd <- tim$solarNoon
        }
        radialPolygon(dailyFraction(tim$sunrise),dailyFraction(tim$sunriseEnd),limits[1],limits[2], col=rgb(1,0.5,0,1))
      }
    }
    if (is.null(plot) |"Sunset" %in% plot) {
      if (!is.na(tim$sunset)) {
        if (is.na(tim$sunsetStart)) {
          tim$sunsetStart <- tim$solarNoon
        }
        radialPolygon(dailyFraction(tim$sunsetStart),dailyFraction(tim$sunset),limits[1],limits[2], col=rgb(1,0.5,0,1))
      }
    }
    if ("Solar Noon" %in% plot) {
      radialPolygon(dailyFraction(tim$solarNoon), dailyFraction(tim$solarNoon),limits[1],limits[2], col=rgb(1,0.5,0,1))
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

#' @export
dielRings <- function(names, starts, ends, cols = "grey", format="HHMM", limits=c(1,2), legend=T) {
  cols <- rep_len(cols, length.out = length(names))
  #Convert to fractional circle
  if (format=="HHMM") {
    starts <- convert2fractionalCircle(starts, input="HHMM")
    ends <- convert2fractionalCircle(ends, input="HHMM")
  }

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

