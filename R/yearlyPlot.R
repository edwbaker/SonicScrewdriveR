#' @export
yearlyLabels <- function(format="months") {
  if (format=="months") {
    ret <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  }
}

#' @export
yearlyPositions <- function(year=2022, format="months") {
  if (isLeapYear(year)) {
    FebDays <- 29
    YearDays <- 366
  } else {
    FebDays <- 28
    YearDays <- 365
  }
  days <- c(0, 31, FebDays+31, FebDays+62, FebDays+92,
            FebDays+123, FebDays+153, FebDays+184, FebDays+215,
            FebDays+245, FebDays+276, FebDays+306)
  if (format == "months") {
    ret <- 2*pi * days / YearDays
  }
  if (format == "mid-months") {
    diffs <- diff(c(days, 360))
    days <- days + 0.5*diffs
    ret <- 2*pi * days / YearDays
  }
  return(ret)
}

isLeapYear <- function(year) {
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

#' @param unit If set to radians outputs a position around a circle. If set to fraction outputs the raw fraction.
#' @export
yearlyFraction <- function(t, year=2022, input="POSIXlt", unit="radians") {
  if (isLeapYear(year)) {
    dc <- 366
  } else {
    dc <- 365
  }
  if (input=="POSIXlt") {
    t <- unclass(as.POSIXlt(t))
    f <- t$yday/dc
  } else if (input=="day") {
    f <- day/dc
  }

  if (unit=="radians") {
    return(2*pi*f)
  }
  return(f)
}

#' @export
emptyYearly <- function(year=2022) {
  plotrix::radial.plot(
    lengths=0,
    radial.pos=0,
    rp.type="p",
    radial.lim=c(0,1,2),
    start=pi,
    label.pos = yearlyPositions(year=year),
    labels=yearlyLabels(),
    clockwise=T,
    poly.col=rgb(0.2,0.2,0.2,1),
    lty=0,
    show.grid.labels =F
  )
}

#' Create a yearly plot
#'
#' ToDO......
#'
#' @param year Year to
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.plot.
#' @param plot Character vector of components to plot
#' @param limits Plotting limits of the daylight regions, default to c(1,2)
#' @param method Plotting library to use
#' @param legend Whether to show a legend
#' @export
#' @importFrom suncalc getSunlightPosition getSunlightTimes
yearlyPlot <- function(year=2022, lat, lon, limits=c(0,2), plot=NULL, method="plotrix", legend=F) {
  start <- as.POSIXlt(paste0(year,"-01-01"))
  end <- as.POSIXlt(paste0(year,"-12-31"))
  dates <- seq.POSIXt(from=start, to=end, by="day")

  tim <- getSunlightTimes(date = as.Date(dates), lat = lat, lon = lon)
  suntime <- as.numeric(difftime(tim$sunset, tim$sunrise, units="mins"))
  suntime <- suntime / (24*60)

  night <- rep_len(1, length.out=length(dates))


  if (method=="plotrix") {
    #Scale for limits
    night <- night * (limits[2]-limits[1])
    suntime <- suntime * (limits[2]-limits[1])

    if (!package.installed("plotrix")){stop("Plotrix must be installed to plot using Plotrix.")}
    emptyYearly()
    angs <- (1:length(suntime))*2*pi/length(suntime)
    angs[length(angs)] <- 2*pi
    angs[1] <- 0
    radialPolygon(NA,angs,limits[1],limits[1]+suntime,col=rgb(1,1,0.6, 0.6))
    radialPolygon(angs,NA,limits[1]+suntime, limits[2], col=rgb(0.8,0.8,0.8,0.8))
  }
}

