#' Phase of day
#'
#' Given a start time and (optionally) a duration returns the phase of day at a given
#' location. This is primarily used to calculate phase of day information for soundscape
#' recording projects.
#'
#' @param time A time object representing the start time of a recording
#' @param duration Duration of recording in seconds
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#' @export
#' @return Data frame of day phases with absolute timestamps and relative times within file
#' @examples
#' dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=40000)
#'
dayPhase <- function(time=Sys.time(), duration=40000, lat=50.1, lon=1.83, tz="UTC") {
  duration <- validateTimeInSeconds(duration)
  etime <- time + duration

  #A recording may run past midnight, so every day it can touch is gathered. The
  #day before is included because the night phase of a day starts on that day.
  days <- seq(as.Date(time, tz=tz) - 1, as.Date(etime, tz=tz) + 1, by=1)
  dt <- do.call(rbind, lapply(days, dayPhases, lat=lat, lon=lon, tz=tz))

  #Two intervals overlap when each starts before the other ends. Testing the three
  #containment cases separately used to miss a phase the recording ended inside.
  rt <- dt[which(dt$Start < etime & dt$End > time), , drop=FALSE]

  relstart <- as.numeric(rt$Start) - as.numeric(time)
  relstart[relstart < 0] <- 0
  relend <- as.numeric(rt$End) - as.numeric(time)
  relend[relend > duration] <- duration

  rt$Start.relative <- as.integer(relstart)
  rt$End.relative <- as.integer(relend)
  return(rt)
}

#' Phases of day
#'
#' Wrapper for suncalc::getSunlightTimes that formats output for this package.
#'
#' Each phase is named for the event that begins it, so that the Dusk.Civil phase
#' runs from sunset to the end of civil twilight. The state of the moon does not
#' divide the day into phases and is attached as the "moon" attribute instead.
#'
#' @param time A time object representing the start time of a recording
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#'
#' @importFrom suncalc getSunlightTimes getMoonIllumination getMoonTimes
#' @return A data frame with a row for each phase of the day and columns giving its
#'   start and end, and a "moon" attribute holding the illumination and whether the
#'   moon is always up or always down.
#' @noRd
dayPhases <- function(time=as.Date(Sys.time()), lat=50.1, lon=1.83, tz="UTC") {
  date <- as.Date(time)
  sc <- getSunlightTimes(date, lat=lat, lon=lon, tz=tz)
  #The night phase runs into the next day, so that day is needed to end it.
  scn <- getSunlightTimes(date + 1, lat=lat, lon=lon, tz=tz)
  mc <- .moonTimes(c(date, date + 1), lat=lat, lon=lon, tz=tz)
  mi <- getMoonIllumination(date)

  #Each phase starts at one solar event and ends at the next.
  events <- c("nightEnd", "nauticalDawn", "dawn", "sunrise", "sunriseEnd",
              "sunsetStart", "sunset", "dusk", "nauticalDusk", "night")
  starts <- as.POSIXct(unlist(sc[events]), tz=tz, origin="1970-01-01")
  ends <- c(starts[-1], as.POSIXct(scn$nightEnd[[1]], tz=tz, origin="1970-01-01"))

  #getMoonTimes reports the rise and set falling within the calendar day, so on most
  #days the moon sets before it rises, and on some there is no set at all. Either
  #way the set that ends the night's moonlight is the following day's.
  moonRise <- mc$rise[[1]]
  moonSet <- mc$set[[1]]
  if (is.na(moonSet) || (!is.na(moonRise) && moonSet < moonRise)) {
    moonSet <- mc$set[[2]]
  }

  ret <- data.frame(
    Start = c(starts, moonRise),
    End = c(ends, moonSet),
    row.names = c(.dayPhaseNames(), "Moon")
  )
  attr(ret, "moon") <- list(
    fraction = mi$fraction[[1]],
    phase = mi$phase[[1]],
    angle = mi$angle[[1]],
    alwaysUp = mc$alwaysUp[[1]],
    alwaysDown = mc$alwaysDown[[1]]
  )
  return(ret)
}

#' Moon rise and set times
#'
#' suncalc::getMoonTimes() raises an error for a single date on which the moon does
#' not both rise and set, which is around one day in fifteen. Asking for a vector of
#' dates returns the correct times, with NA where the event does not occur, so the
#' dates are always requested together.
#'
#' Doing so makes suncalc emit two warnings of its own, about a type coercion and a
#' recycled comparison in its internals. Over a year of dates the vectorised results
#' are identical to the per-date results wherever the latter can be calculated, and
#' the days it cannot are exactly those with one of rise and set missing, so neither
#' warning reflects a wrong answer here. Only those two are silenced.
#'
#' @param dates A vector of dates
#' @param lat Latitude
#' @param lon Longitude
#' @param tz Time-zone
#' @return The data frame returned by suncalc::getMoonTimes().
#' @noRd
.moonTimes <- function(dates, lat, lon, tz) {
  return(withCallingHandlers(
    getMoonTimes(date=dates, lat=lat, lon=lon, tz=tz),
    warning = function(w) {
      known <- c("taken as TRUE when assigning to type",
                 "longer object length is not a multiple of shorter object length")
      if (any(vapply(known, grepl, logical(1), x=conditionMessage(w), fixed=TRUE))) {
        invokeRestart("muffleWarning")
      }
    }
  ))
}

#' Names of the phases of the day
#'
#' In the order the phases occur, each named for the solar event that begins it.
#'
#' @return A character vector of phase names.
#' @noRd
.dayPhaseNames <- function() {
  return(c("Dawn.Astro", "Dawn.Naut", "Dawn.Civil", "Sunrise", "Day",
           "Sunset", "Dusk.Civil", "Dusk.Naut", "Dusk.Astro", "Night"))
}

#' Package names for the columns returned by suncalc::getSunlightTimes
#'
#' Named by the suncalc column rather than by position, as getSunlightTimes does not
#' return its columns in the order the events occur and a positional list silently
#' mislabels them if that order ever changes.
#'
#' @return A named character vector, suncalc name to package name.
#' @noRd
.sunlightColumnNames <- function() {
  return(c(
    date = "Date", lat = "Lat", lon = "Lon",
    nightEnd = "Dawn.Astro", nauticalDawn = "Dawn.Naut", dawn = "Dawn.Civil",
    sunrise = "Sunrise", sunriseEnd = "Day",
    sunsetStart = "Sunset", sunset = "Dusk.Civil", dusk = "Dusk.Naut",
    nauticalDusk = "Dusk.Astro", night = "Night"
  ))
}

#' Phases of days
#'
#' @param date A time object representing the start time of a recording
#' @param period "month" or "year"
#' @param plot If true plots the data, default FALSE
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#' @return A data frame with a row for each day, giving the times of each phase of the day and the state of the moon.
#' @export
#' @importFrom suncalc getSunlightTimes getMoonIllumination getMoonTimes
#' @importFrom hms as_hms
#' @importFrom graphics lines axis
#' @examples
#' daysPhases(date=as.Date("2024-06-15"), period="month")
#'
daysPhases <- function(date=Sys.Date(), period="year", plot=FALSE, lat=50.1, lon=1.83, tz="UTC") {
  halfwidth <- switch(period, year = 180, month = 15,
                      stop(paste("Unknown period for daysPhases:", period)))
  dates <- seq.Date(as.Date(date) - halfwidth, as.Date(date) + halfwidth, by = 1)

  ret <- getSunlightTimes(date = dates, lat = lat, lon = lon, tz = tz)
  mi <- getMoonIllumination(date = dates)
  mt <- .moonTimes(dates, lat = lat, lon = lon, tz = tz)

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

  ret <- cbind(
    ret,
    Moon.Fraction = mi$fraction,
    Moon.Phase = mi$phase,
    Moon.Angle = mi$angle,
    Moonrise = mt$rise,
    Moonset = mt$set,
    Moon.AlwaysUp = mt$alwaysUp,
    Moon.AlwaysDown = mt$alwaysDown
  )

  #Renamed by lookup so that the columns suncalc returns cannot drift out of step
  #with the names given to them.
  map <- .sunlightColumnNames()
  known <- names(ret) %in% names(map)
  names(ret)[known] <- map[names(ret)[known]]
  return(ret)
}
