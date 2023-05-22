#' Phase of day
#'
#' Given a start time and (optionally) a duration returns the phase of day at a given
#' location. This is primarily used to calculate phase of day information for soundscape
#' recording projects.
#'
#' @param time A time object representing the start time of a recording
#' @param duration Duration of recording
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#' @export
#' @return Data frame of day phases with absolute timestamps and relative times within file
#'
dayPhase <- function(time=Sys.time(), duration=40000, lat=50.1, lon=1.83, tz="UTC") {
  d <- dayPhases(as.Date(time), lat=lat, lon=lon, tz=tz)
  dt <- d[1:11,]
  etime <- time + duration
  rt <- subset(dt,
                (time >= dt[,1] & time < dt[,2]) |
                (time < dt[,1] & etime > dt[,2]) |
                (time >= dt[,1] & etime < dt[,2])
              )
  #TODO: add relevant moon data
  stime <- time
  while(max(rt[,2]) < etime) {
    stime <- stime + 86400
    d <- dayPhases(as.Date(time), lat=lat, lon=lon, tz=tz)
    dt <- d[1:11,]
    rt2 <- subset(dt,
                 (stime >= dt[,1] & stime < dt[,2]) |
                   (stime < dt[,1] & etime > dt[,2]) |
                   (stime >= dt[,1] & etime < dt[,2])
    )
    #TODO: add relevant moon data
    rt <- rbind(rt, rt2)
  }
  relstart <- as.numeric(rt[,1]) - as.numeric(time)
  relstart[relstart<0] <- 0
  relend <- as.numeric(rt[,2]) - as.numeric(time)
  relend[relend>etime] <- etime
  cn <- colnames(rt)
  rt <- cbind(rt, as.integer(relstart), as.integer(relend))
  colnames(rt) <- c(cn, "Start.relative", "End.relative")
  return(rt)
}

#' Phases of day
#'
#' Wrapper for suncalc::getSunlightTimes that formats output for this package.
#'
#' @param time A time object representing the start time of a recording
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#'
#' @importFrom suncalc getSunlightTimes getMoonIllumination getMoonTimes
dayPhases <- function(time=as.Date(Sys.time()), lat=50.1, lon=1.83, tz="UTC") {
  sc <- getSunlightTimes(as.Date(time), lat=lat, lon=lon, tz=tz)
  #Also load next day to find out when night ends
  #TODO: What happens when this is the same day?
  #Moon rise and set might need to be previous day or next day?
  scn <- getSunlightTimes(as.Date(as.POSIXlt(time) + 86400) , lat=lat, lon=lon, tz=tz)
  mc <- getMoonTimes(as.Date(time), lat=lat, lon=lon, tz=tz)
  mi <- getMoonIllumination(as.Date(time))
  rn <- c("Dawn.Astro", "Dawn.Naut", "Dawn.Civil", "Sunrise", "Day", "Sunset", "Dusk.Civil", "Dusk.Naut", "Dusk.Astro", "Night",
          "Moon", "Moon.AlwaysUp", "Moon.AlwaysDown",
          "Moon.Fraction", "Moon.Phase", "Moon.Angle"
          )
  cn <- c("Start", "End")

  starts <- c(
    sc$nightEnd[[1]],
    sc$nauticalDawn[[1]],
    sc$dawn[[1]],
    sc$sunrise[[1]],
    sc$sunriseEnd[[1]],
    sc$sunsetStart[[1]],
    sc$sunset[[1]],
    sc$dusk[[1]],
    sc$nauticalDusk[[1]],
    sc$night[[1]],
    mc$rise[[1]],
    mc$alwaysUp[[1]],
    mc$alwaysDown[[1]],
    mi$fraction[[1]],
    mi$phase[[1]],
    mi$angle[[1]]
  )
  ends <- c(
    sc$nauticalDawn[[1]],
    sc$dawn[[1]],
    sc$sunrise[[1]],
    sc$sunriseEnd[[1]],
    sc$sunsetStart[[1]],
    sc$sunset[[1]],
    sc$dusk[[1]],
    sc$nauticalDusk[[1]],
    sc$night[[1]],
    scn$nightEnd,
    mc$set[[1]],
    mc$alwaysUp[[1]],
    mc$alwaysDown[[1]],
    mi$fraction[[1]],
    mi$phase[[1]],
    mi$angle[[1]]

  )
  ret <- cbind(starts, ends)
  colnames(ret) <- cn
  rownames(ret) <- rn
  return(ret)
}

#' Phases of days
#'
#' @param date A time object representing the start time of a recording
#' @param period "month" or "year"
#' @param plot If true plots the data, default FALSE
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#' @export
#' @importFrom suncalc getSunlightTimes getMoonIllumination getMoonTimes
#' @importFrom hms as_hms
#' @importFrom graphics lines axis
daysPhases <- function(date=Sys.Date(), period="year", plot=FALSE, lat=50.1, lon=1.83, tz="UTC") {
  if (period == "year") {
    ret <- getSunlightTimes(date = seq.Date(Sys.Date()-180, Sys.Date()+180, by = 1), lat = lat, lon = lon, tz = tz)
    mi <- getMoonIllumination(date = seq.Date(Sys.Date()-180, Sys.Date()+180, by=1))
    mt <- getMoonTimes(date = seq.Date(Sys.Date()-180, Sys.Date()+180, by = 1), lat = lat, lon = lon, tz = tz)
    ret <-cbind(ret, mi$fraction, mi$phase, mi$angle, mt$rise, mt$set, mt$alwaysUp, mt$alwaysDown)
  }
  if (period == "month") {
    ret <- getSunlightTimes(date = seq.Date(Sys.Date()-15, Sys.Date()+15, by = 1), lat = lat, lon = lon, tz = tz)
    mi <- getMoonIllumination(date = seq.Date(Sys.Date()-15, Sys.Date()+15, by=1))
    mt <- getMoonTimes(date = seq.Date(Sys.Date()-180, Sys.Date()+180, by = 1), lat = lat, lon = lon, tz = tz)
    ret <-cbind(ret, mi$fraction, mi$phase, mi$angle, mt$rise, mt$set, mt$alwaysUp, mt$alwaysDown)
  }

  if (plot) {
    plot(ret$date,
         as_hms(ret$nightEnd),
         type="l",
         ylim=c(0,86400),
         xlab="Date",
         yaxt="n",
         ylab="Time of Day",

         )
    lines(ret$date, as_hms(ret$nauticalDawn), type="l", col="red")
    lines(ret$date, as_hms(ret$dawn), type="l", col="blue")
    lines(ret$date, as_hms(ret$sunrise), type="l", col="green")
    lines(ret$date, as_hms(ret$sunriseEnd), type="l", col="green")
    lines(ret$date, as_hms(ret$solarNoon), type="l", col="purple")
    lines(ret$date, as_hms(ret$sunsetStart), type="l", col="green")
    lines(ret$date, as_hms(ret$sunset), type="l", col="green")
    lines(ret$date, as_hms(ret$dusk), type="l", col="blue")
    lines(ret$date, as_hms(ret$nauticalDusk), type="l", col="red")
    lines(ret$date, as_hms(ret$night), type="l")
    axis(2, at = plotHMS.at(), labels=plotHMS.lab(), las=2)
  }
  cn <- c("Date", "Lat", "Lon", "solarNoon", "nadir", "Sunrise", "Dusk.Civil", "Day", "Sunset", "Dawn.Civil", "Dusk.Civil", "Dawn.Naut", "Dusk.Naut", "Dawn.Astro", "Night", "goldenHourEnd", "GoldenHour",
          "Moon.Fraction", "Moon.Phase", "Moon.Angle",
          "Moonrise", "Moonset", "Moon.AlwaysUp", "Moon.AlwaysDown"
          )
  colnames(ret) <- cn
  return(ret)
}
