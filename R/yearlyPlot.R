yearlyLabels <- function(format="months", pos=NULL) {
  if (is.null(pos)) {
    if (format=="months") {
      ret <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    }
  }
  else {
    ret <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)
  }
  return(ret)
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
yearlyPlot <- function(year, lat, lon, limits=c(0,2), plot=NULL, method="plotrix", legend=F) {
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
    if (limits[1] == 0) {
      plotrix::radial.plot(
        lengths=night,
        radial.pos=2*pi*seq_along(dates)/length(dates),
        rp.type="p",
        radial.lim=c(0,1,2),
        start=pi,
        label.pos = yearlyLabels(pos='pos')*pi/180,
        labels=yearlyLabels(),
        clockwise=T,
        poly.col=rgb(0.6,0.6,0.6,0.5),
        lty=0,
        show.grid.labels =F
      )
      plotrix::radial.plot(
        lengths=suntime,
        radial.pos=2*pi*seq_along(dates)/length(dates),
        rp.type="p",
        radial.lim=c(0,1,2),
        start=pi,
        poly.col=rgb(1,1,0.6,0.6),
        lty=0,
        add=TRUE
      )
    } else {
      plotrix::radial.plot(
        lengths=0,
        radial.pos=0,
        rp.type="p",
        radial.lim=c(0,1,2),
        start=pi,
        label.pos = yearlyLabels(pos='pos')*pi/180,
        labels=yearlyLabels(),
        clockwise=T,
        poly.col=rgb(0.2,0.2,0.2,0.5),
        lty=0,
        show.grid.labels =F
      )
      for (i in 1:length(day)) {
        if (i==1) {
          j <- length(day)
        } else {
          j <- i-1
        }
        i_ang <- i*2*pi/length(day) - pi
        j_ang <- j*2*pi/length(day) - pi
        plotrix::drawSectorAnnulus(j_ang, i_ang, limits[1], limits[1]+(day[i]+ day[j])/2, col=rgb(1,1,0, 0.6), angleinc=0.05)
      }
    }
  }
}

