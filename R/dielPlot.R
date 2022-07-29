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

dielFraction <- function(t, unit="radians", start=pi, clockwise=FALSE) {
  t <- unclass(as.POSIXlt(t))
  f <- (t$sec + 60*t$min + 3600*t$hour)/86400
  if (unit == "radians") {
    f <- start + f*2*pi
    if (clockwise) {

    } else {
      return(2*pi - f)
    }
  }
  if (unit == "unit") {
    return(f)
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
  times <- seq.POSIXt(from=date, by="min", length.out=60*24)
  attr(times, 'tzone') <- "UTC"
  #Calculate night time from sun altitude above horizon
  pos <- getSunlightPosition(date = times, lat = lat, lon = lon, keep = c("altitude"))
  tim <- getSunlightTimes(date = as.Date(times[1]), lat = lat, lon = lon)
  day <- pos$altitude*2/pi
  day[which(day < 0)] <- 0


  if (method=="plotrix") {
    if (!package.installed("plotrix")){stop("Plotrix must be installed to plot using Plotrix.")}
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

    alt <- getSunlightPosition(tim$solarNoon, lat=tim$lat, lon=tim$lon, keep=c("altitude"))$altitude

    leg <- c()
    col <- c()

    if (is.null(plot) | "Sunrise" %in% plot) {
      if (!is.na(tim$sunrise)) {
        if (is.na(tim$sunriseEnd)) {
          tim$sunriseEnd <- tim$solarNoon
        }
        plotrixSectorAnnulus(dielFraction(tim$sunrise),dielFraction(tim$sunriseEnd),limits[1],limits[2], col=rgb(1,0.5,0,1))
      }
    }
    if (is.null(plot) |"Sunset" %in% plot) {
      if (!is.na(tim$sunset)) {
        if (is.na(tim$sunsetStart)) {
          tim$sunsetStart <- tim$solarNoon
        }
        plotrixSectorAnnulus(dielFraction(tim$sunsetStart),dielFraction(tim$sunset),limits[1],limits[2], col=rgb(1,0.5,0,1))
      }
    }
    if ("Solar Noon" %in% plot) {
      plotrixSectorAnnulus(dielFraction(tim$solarNoon), dielFraction(tim$solarNoon),limits[1],limits[2], col=rgb(1,0.5,0,1))
    }

    if (is.null(plot) |"Civil Twilight" %in% plot) {
      leg <- c(leg, "Civil Twilight")
      col <- c(col, rgb(0.8,0.8,0.8,1))
      if (!is.na(tim$sunrise)) {
        if (dielFraction(tim$sunrise) <= 0) {
          if (dielFraction(tim$sunset) <= 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$sunrise),pi, limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
            plotrix::drawSectorAnnulus(dielFraction(tim$sunset),-pi, limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(dielFraction(tim$sunset), dielFraction(tim$sunrise),limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
          }
        } else {
          if (dielFraction(tim$sunrise) > 0 & dielFraction(tim$sunset) > 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$sunset), dielFraction(tim$sunrise),limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(pi, dielFraction(tim$sunrise), limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
            plotrix::drawSectorAnnulus(-pi, dielFraction(tim$sunset),  limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
          }
        }
      }
      if (is.na(tim$sunrise) & !is.na(tim$dawn)) {
        plotrix::drawSectorAnnulus(pi, -pi,limits[1],limits[2], col=rgb(0.8,0.8,0.8,1), angleinc=0.01)
      }
    }
    if (is.null(plot) |"Nautical Twilight" %in% plot) {
      leg <- c(leg, "Nautical Twilight")
      col <- c(col, rgb(0.6,0.6,0.6,1))
      if (!is.na(tim$dawn)) {
        if (dielFraction(tim$dawn) <= 0) {
          if (dielFraction(tim$dusk) <= 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$dawn), -dielFraction(tim$dusk),limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(dielFraction(tim$dusk), dielFraction(tim$dawn),limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
          }
        } else {
          if (dielFraction(tim$dawn) > 0  & dielFraction(tim$dusk) > 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$dusk), dielFraction(tim$dawn),limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(pi, dielFraction(tim$dawn),limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
            plotrix::drawSectorAnnulus(-pi, dielFraction(tim$dusk),limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
          }
        }
      }
      if (is.na(tim$dawn) & !is.na(tim$nauticalDawn)) {
        plotrix::drawSectorAnnulus(pi, -pi,limits[1],limits[2], col=rgb(0.6,0.6,0.6,1), angleinc=0.01)
      }
    }
    if (is.null(plot) |"Astronomical Twilight" %in% plot) {
      leg <- c(leg, "Astronomical Twilight")
      col <- c(col, rgb(0.4,0.4,0.4,1))
      if (!is.na(tim$nauticalDawn)) {
        if (dielFraction(tim$nauticalDawn) <= 0) {
          if (dielFraction(tim$nauticalDawn) <= 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$nauticalDusk), dielFraction(tim$nauticalDawn),limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(dielFraction(tim$nauticalDusk), dielFraction(tim$nauticalDawn),limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)
          }
        } else {
          if (dielFraction(tim$nauticalDawn) > 0 & dielFraction(tim$nauticalDusk) > 0) {
            plotrix::drawSectorAnnulus(dielFraction(tim$nauticalDusk), dielFraction(tim$nauticalDawn),limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)
          } else {
            plotrix::drawSectorAnnulus(pi, dielFraction(tim$nauticalDawn),limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)
            plotrix::drawSectorAnnulus(-pi, dielFraction(tim$nauticalDusk),limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)

          }
        }
      }
      if (is.na(tim$nauticalDawn) & !is.na(tim$night)) {
        plotrix::drawSectorAnnulus(pi, -pi,limits[1],limits[2], col=rgb(0.4,0.4,0.4,1), angleinc=0.01)
      }
    }
    if (is.null(plot) |"Night" %in% plot) {
      leg <- c(leg, "Night")
      col <- c(col, rgb(0.2,0.2,0.2,1))
      if (!is.na(tim$night) & !is.na(tim$nightEnd)){
        if (dielFraction(tim$nightEnd) <= 0) {
          plotrix::drawSectorAnnulus(dielFraction(tim$night), dielFraction(tim$nightEnd),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), angleinc=0.01)
        } else if (dielFraction(tim$night) >= 0) {
          plotrix::drawSectorAnnulus(dielFraction(tim$night), dielFraction(tim$nightEnd),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), angleinc=0.01)
        } else{
          plotrix::drawSectorAnnulus(pi, dielFraction(tim$nightEnd),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), angleinc=0.01)
          plotrix::drawSectorAnnulus(-pi, dielFraction(tim$night),limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), angleinc=0.01)
        }

      }
      if(alt <= -0.314159) {
        if (is.na(tim$night)) {
          plotrix::drawSectorAnnulus(pi, -pi,limits[1],limits[2], col=rgb(0.2,0.2,0.2,1), angleinc=0.01)
        }
      }
    }
    if ("Nadir" %in% plot) {
      plotrix::drawSectorAnnulus(dielFraction(tim$nadir), dielFraction(tim$nadir),limits[1],limits[2], col=rgb(0,0,0,1), angleinc=0.01)
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

plotrixSectorAnnulus <- function(from,to,radius1,radius2,col,angleinc=0.03) {
  if (to <= 0) {
    plotrix::drawSectorAnnulus(to,from,radius1,radius2,col,angleinc=0.01)
  } else if (from >= 0) {
    plotrix::drawSectorAnnulus(from,to,radius1,radius2,col,angleinc=0.01)
  } else{
    plotrix::drawSectorAnnulus(pi,to,radius1,radius2,col,angleinc=0.01)
    plotrix::drawSectorAnnulus(-pi,from,radius1,radius2,col,angleinc=0.01)
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
    plotrixSectorAnnulus(starts[i],ends[i], arcs[i],arcs[i]+0.1, col=cols[i], angleinc=0.01)
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

