#' Generate labels for a yearly plot
#'
#' Generates monthly labels for a yearlyPlot().
#' @return A character vector of twelve month abbreviations.
#' @export
yearlyLabels <- function() {
  ret <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  return(ret)
}

#' Generate positions of labels for a yearly plot
#'
#' Generates positions for monthly labels of a yearlyPlot() in radians. The positions can either be for the
#' start of the month, or middle of the month.
#'
#' The function allows for leap years if the year parameter is provided.
#' @param year Year to calculate
#' @param format One of months, mid-months, days
#' @return A numeric vector of label positions, in radians.
#' @export
yearlyPositions <- function(year=2022, format="months") {
  .validateChoice(format, c("months", "mid-months", "days"), msg=paste("Unknown format:", format))
  if (.isLeapYear(year)) {
    FebDays <- 29
    YearDays <- 366
  } else {
    FebDays <- 28
    YearDays <- 365
  }
  if (format=="days") {
    ret <- 2*pi * (1:YearDays)/YearDays
  } else {
    days <- c(0, 31, FebDays+31, FebDays+62, FebDays+92,
              FebDays+123, FebDays+153, FebDays+184, FebDays+215,
              FebDays+245, FebDays+276, FebDays+306)
    if (format == "months") {
      ret <- 2*pi * days / YearDays
    }
    if (format == "mid-months") {
      #The last month runs to the end of the year, which is not 360 days.
      diffs <- diff(c(days, YearDays))
      days <- days + 0.5*diffs
      ret <- 2*pi * days / YearDays
    }
  }
  return(ret)
}

.isLeapYear <- function(year) {
  year <- as.numeric(year)
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#' Calculate the fraction of a year given by a value
#'
#' Given an object that can be coerced to POSIXlt, return the fraction of a year represented by the object.
#'
#' @param t Object to be converted to a fraction
#' @param year Year to calculate fractions of (allows for leap years)
#' @param input One of POSIXlt (default)
#' @param unit If set to radians outputs a position around a circle. If set to fraction outputs the raw fraction.
#' @return The position of the date within the year, in radians or as a fraction of a year.
#' @export
yearlyFraction <- function(t, year=2022, input="POSIX", unit="radians") {
  .validateChoice(input, c("POSIX", "day"), msg=paste("Unknown input:", input))
  if (.isLeapYear(year)) {
    dc <- 366
  } else {
    dc <- 365
  }
  if (input=="POSIX") {
    t <- unclass(as.POSIXlt(t))
    f <- t$yday/dc
  } else if (input=="day") {
    f <- t/dc
  }

  if (unit=="radians") {
    return(2*pi*f)
  }
  return(f)
}


#' Create an empty yearly plot
#'
#' Create a yearly plot with labels but without sun or night duration plotted.
#' @param year Year to plot (allows for leap years)
#' @param method Plotting package to use
#' @param rot Rotation of the origin (defaults to pi)
#' @return Called for its side effect of drawing a plot. The return value is that of the underlying plotting function and should not be relied on.
#' @export
emptyYearly <- function(year=2022, method="plotrix", rot=pi) {
  .validateChoice(method, .dielPlotMethods(), "method", "emptyYearly", prep="for")
  if (!package.installed("plotrix")) {
    stop("Plotrix must be installed to plot using Plotrix.")
  }
  plotrix::radial.plot(
    lengths=0,
    radial.pos=0,
    rp.type="p",
    radial.lim=c(0,1,2),
    start=rot,
    label.pos = yearlyPositions(year=year),
    labels=yearlyLabels(),
    clockwise=TRUE,
    poly.col=rgb(0.2,0.2,0.2,1),
    lty=0,
    show.grid.labels =FALSE
  )
}

#' Create a yearly plot
#'
#' ToDO......
#'
#' @param year Year to plot (allows for leap years).
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @param plot Character vector of components to plot
#' @param limits Plotting limits of the daylight regions, default to c(1,2)
#' @param method Plotting library to use
#' @param legend Whether to show a legend
#' @return Called for its side effect of drawing a plot. The return value is that of the underlying plotting function and should not be relied on.
#' @export
#' @importFrom suncalc getSunlightPosition getSunlightTimes
yearlyPlot <- function(year=2022, lat, lon, limits=c(0,2), plot=NULL, method="plotrix", legend=FALSE) {
  if (!is.null(plot)) {
    warning("The plot argument of yearlyPlot() is not implemented, and is ignored.")
  }
  .validateChoice(method, .dielPlotMethods(), "method", "yearlyPlot", prep="for")
  if (!package.installed("plotrix")) {
    stop("Plotrix must be installed to plot using Plotrix.")
  }

  #Dates throughout. Building them as times in whatever zone the session was in
  #and converting back with as.Date(), which reads them as UTC, moved every day
  #by that zone's offset.
  dates <- seq.Date(
    as.Date(paste0(year, "-01-01")),
    as.Date(paste0(year, "-12-31")),
    by = "day"
  )

  tim <- getSunlightTimes(date = dates, lat = lat, lon = lon, tz = "UTC")
  suntime <- as.numeric(difftime(tim$sunset, tim$sunrise, units="mins")) / (24*60)

  #Above the polar circles the sun may not rise or set at all, and suncalc gives
  #NA for both. Whether that is a day with no night or a night with no day is
  #settled by where the sun is at noon. Left as NA these reached polygon(), which
  #reads a missing coordinate as a break between sub-polygons and silently drew
  #the wrong shape.
  polar <- is.na(suntime)
  if (any(polar)) {
    altitude <- getSunlightPosition(date=tim$solarNoon[polar], lat=lat, lon=lon)$altitude
    suntime[polar] <- ifelse(altitude > 0, 1, 0)
  }

  #Scale for limits
  suntime <- suntime * (limits[2]-limits[1])

  emptyYearly(year=year)

  #One angle per day, starting at zero, with the first day repeated at the end so
  #that the ring closes. Counting from one put day one at zero but day two a whole
  #extra day around the circle.
  angs <- (0:length(suntime)) * 2*pi / length(suntime)
  suntime <- circularise(suntime)

  day.col <- rgb(1, 1, 0.6, 0.6)
  night.col <- rgb(0.8, 0.8, 0.8, 0.8)
  radialPolygon(NA, angs, limits[1], limits[1]+suntime, col=day.col)
  radialPolygon(angs, NA, limits[1]+suntime, limits[2], col=night.col)

  #Placed as dielPlot() places its own legend.
  if (legend) {
    .polarLegend(c("Day", "Night"), c(day.col, night.col))
  }
}
