#' Phase of day
#'
#' Given a start time and (optionally) a duration returns the phase of day at a given
#' location. This is primarily used to calculate phase of day information for soundscape
#' recording projects.
#'
#' Each phase is named for the solar event that begins it, so that the Dusk.Civil
#' phase runs from sunset to the end of civil twilight, and the phases returned
#' account for the whole of the recording. A phase that happens twice within one
#' recording is numbered, as Day and Day1.
#'
#' The phases of the day do not all happen everywhere. At 50 degrees north in
#' midsummer the sun never reaches astronomical twilight, so there is no Night, and
#' the Dusk.Astro phase runs from the end of nautical twilight through to the next
#' morning; in polar day and polar night a single phase can last for months, and is
#' named for the sun's altitude through it. A phase therefore need not start or end
#' on the day of the recording, and the start and end given for it are the real
#' ones, however far outside the recording they fall.
#'
#' The moon does not divide the day into phases and is reported alongside them, as
#' one row for each night of moonlight the recording falls in.
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
  #A Date is a count of days, so time + duration below would add the duration as
  #days rather than seconds, and the overlap filter would then compare epoch
  #seconds against day counts, which warns rather than stops and returns the
  #wrong answer entirely. Both sibling functions in this file take a Date, so
  #one arrives here easily.
  if (!inherits(time, "POSIXt")) {
    stop(
      "time must be a POSIXct or POSIXlt, not ", class(time)[1],
      ". A recording starts at an instant rather than on a day: give ",
      "as.POSIXct(\"2024-06-15 20:00:00\", tz=\"UTC\"), or use dayPhases() or ",
      "daysPhases() for whole days."
    )
  }
  duration <- validateTimeInSeconds(duration)
  etime <- time + duration

  #The solar phases are built from the run of events themselves rather than day by
  #day, so that a phase bounded by an event that does not occur still has both of
  #its ends. A recording may run past midnight, so every day its moonlight can
  #touch is gathered. The days before it are included because the moonlight falling
  #on a recording began when the moon rose, which is the evening before on most
  #nights and, where the moon stays up for days on end as it does at high
  #latitudes, as much as a fortnight before.
  days <- seq(as.Date(time, tz=tz) - 16, as.Date(etime, tz=tz) + 1, by=1)
  dt <- rbind(
    .solarPhases(time, etime, lat=lat, lon=lon, tz=tz),
    .moonPhases(days, lat=lat, lon=lon, tz=tz)
  )

  #Two intervals overlap when each starts before the other ends. Testing the three
  #containment cases separately used to miss a phase the recording ended inside.
  rt <- dt[which(dt$Start < etime & dt$End > time), , drop=FALSE]
  #A phase that happens more than once within the recording is numbered, counting
  #only the phases returned: the numbering of the days searched to find them, which
  #at high latitudes runs into the hundreds, is no business of the caller's.
  rownames(rt) <- make.unique(sub("[0-9]+$", "", rownames(rt)), sep="")

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
#' A phase whose own event does not occur at this latitude and date does not start
#' on this day and is given as NA. The phase then in effect began on an earlier day
#' and runs on through this one: at 50 degrees north in midsummer the sun never
#' reaches astronomical twilight, so Night does not start and the Dusk.Astro phase
#' of the day before covers the short night. dayPhase() works from the run of
#' events rather than from one day's table and so reports those phases.
#'
#' @param time A time object representing the start time of a recording
#' @param lat Latitude of recording device
#' @param lon Longitude of recording device
#' @param tz Time-zone of recording device when recording was made
#'
#' @importFrom suncalc getSunlightTimes getSunlightPosition getMoonIllumination getMoonTimes
#' @return A data frame with a row for each phase of the day and columns giving its
#'   start and end, and a "moon" attribute holding the illumination and whether the
#'   moon is always up or always down.
#' @noRd
dayPhases <- function(time=as.Date(Sys.time()), lat=50.1, lon=1.83, tz="UTC") {
  #as.Date() reads a POSIXct in UTC unless told otherwise, so a moment that
  #falls on one day where the recorder stands and on another in UTC was given
  #the phases of the UTC day rather than of the day asked for.
  date <- as.Date(time, tz=tz)
  dayStart <- .startOfDay(date, tz=tz)
  dayEnd <- .startOfDay(date + 1, tz=tz)

  #The phases covering the day, each running from its own event to the next event
  #that occurs, which for the last phase of the day is in the day that follows.
  ph <- .solarPhases(dayStart, dayEnd, lat=lat, lon=lon, tz=tz)
  #A phase belongs to this day when the event that begins it falls within the day.
  starts <- as.numeric(ph$Start)
  own <- ph[starts >= as.numeric(dayStart) & starts < as.numeric(dayEnd), , drop=FALSE]
  #Several days of phases can be returned, so they are matched by name with the
  #numbering that repeated names carry removed.
  found <- match(.dayPhaseNames(), sub("[0-9]+$", "", rownames(own)))

  mc <- .moonTimes(c(date, date + 1), lat=lat, lon=lon, tz=tz)
  mi <- getMoonIllumination(date)

  ret <- rbind(
    data.frame(
      Start = own$Start[found],
      End = own$End[found],
      row.names = .dayPhaseNames()
    ),
    .moonPhases(date, lat=lat, lon=lon, tz=tz)
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

#' Solar phases covering a stretch of time
#'
#' Each phase runs from the solar event that begins it to the next event that
#' occurs. An event that does not happen at the latitude and date asked for is
#' given by suncalc as NA, and taking each phase to end at the next event in the
#' day's table put an NA at one end of every phase such an event bounded, which the
#' overlap test in dayPhase() then dropped without a word: at the package default
#' location in midsummer there is no astronomical night, and the four hours of
#' twilight that Night would have divided belonged to no phase at all. The
#' truthful statement is that a phase whose event does not occur never starts, and
#' that the phase in effect runs on to the next event that does occur, which can be
#' the next day or months later.
#'
#' Finding the phase a stretch of time sits in therefore means looking outside it,
#' so the window of days is widened until it holds an event on either side of the
#' stretch. Where no event bounds it the sun has crossed none of the thresholds
#' that divide the phases, and .altitudePhase() names the one phase it is in.
#'
#' @param from Start of the stretch of time
#' @param to End of the stretch of time
#' @param lat Latitude
#' @param lon Longitude
#' @param tz Time-zone
#' @return A data frame of phases in order, covering the whole of from to to, with
#'   columns Start and End and a row named for each phase.
#' @noRd
.solarPhases <- function(from, to, lat, lon, tz) {
  d0 <- as.Date(from, tz=tz)
  d1 <- as.Date(to, tz=tz)
  #A day either side is enough wherever the sun crosses every threshold, which is
  #everywhere below about 49 degrees of latitude and most of the year above it. The
  #wider span is only reached at high latitudes, and covers the longest polar day
  #there is, which lasts around 186 days at the pole. Nothing is gained by trying
  #spans in between, as suncalc costs much the same for one date as for hundreds.
  for (span in c(1, 200)) {
    ev <- .solarEvents(seq(d0 - span, d1 + span, by=1), lat=lat, lon=lon, tz=tz)
    if (nrow(ev) > 0 && min(ev$Start) <= from && max(ev$Start) >= to) {
      break
    }
  }

  if (nrow(ev) == 0) {
    return(data.frame(
      Start = from,
      End = to,
      row.names = .altitudePhase(from, to, lat=lat, lon=lon)
    ))
  }

  n <- nrow(ev)
  if (max(ev$Start) >= to) {
    #The last event found starts after the stretch, so it ends the phase before it
    #and begins one that is of no interest here.
    starts <- ev$Start[-n]
    ends <- ev$Start[-1]
    phases <- ev$Phase[-n]
  } else {
    #No event follows the stretch within the widest window searched, so the last
    #phase is only known to reach the end of it.
    starts <- ev$Start
    ends <- c(ev$Start[-1], to)
    phases <- ev$Phase
  }

  #Within a phase bounded by two events the sun stays in the band its own event
  #opened, as leaving that band would itself be an event. A phase that runs for
  #longer than a day is one whose closing event never came, and there the sun does
  #wander: the midnight sun at 70 degrees north begins with the sun's disc touching
  #the horizon, which is the Sunset phase, and then climbs away from it for two
  #months. Such a phase is named for where the sun actually is.
  long <- which(as.numeric(difftime(ends, starts, units="secs")) > 86400)
  for (i in long) {
    phases[[i]] <- .altitudePhase(starts[[i]], ends[[i]], lat=lat, lon=lon)
  }

  ret <- data.frame(Start=starts, End=ends, row.names=make.unique(phases, sep=""))
  if (min(ev$Start) > from) {
    #Nor does any event precede the stretch, so the phase it opens with is only
    #known to reach back to the start of it, and nothing has named it yet.
    ret <- rbind(
      data.frame(
        Start = from,
        End = min(ev$Start),
        row.names = .altitudePhase(from, min(ev$Start), lat=lat, lon=lon)
      ),
      ret
    )
  }
  return(ret)
}

#' Solar events over a run of days
#'
#' @param dates A vector of dates
#' @param lat Latitude
#' @param lon Longitude
#' @param tz Time-zone
#' @return A data frame of the events that occur, in the order they occur, giving
#'   the time of each in Start and the name of the phase it begins in Phase.
#' @noRd
.solarEvents <- function(dates, lat, lon, tz) {
  phases <- .solarEventPhases()
  sc <- getSunlightTimes(date=dates, lat=lat, lon=lon, tz=tz)
  #unlist() takes one column at a time, so each phase name covers a whole column.
  times <- as.POSIXct(unlist(sc[names(phases)]), tz=tz, origin="1970-01-01")
  ret <- data.frame(
    Start = times,
    Phase = rep(unname(phases), each=nrow(sc)),
    row.names = NULL
  )
  #An event suncalc gives as NA is one that does not happen, and so is no event.
  ret <- ret[!is.na(ret$Start), , drop=FALSE]
  ret <- ret[order(ret$Start), , drop=FALSE]
  #On the day the sun stops setting suncalc repeats the previous day's sunriseEnd,
  #and one event cannot happen twice.
  return(ret[!duplicated(ret), , drop=FALSE])
}

#' The phase a stretch of time is in, taken from the sun's altitude
#'
#' For naming a stretch of time that no pair of events divides, which is polar day,
#' polar night, and the weeks of unbroken twilight between them. dayPhase() used to
#' return no solar phase at all for those. The sun is where it is for the whole of
#' such a stretch, give or take the turn at either end of it, so its altitude
#' halfway through names the phase; and the phase is named for the band of altitude
#' as everywhere else, a sun held between 12 and 18 degrees below the horizon being
#' in astronomical twilight, which is the Dusk.Astro phase.
#'
#' @param from Start of the stretch of time
#' @param to End of the stretch of time
#' @param lat Latitude
#' @param lon Longitude
#' @return The name of the phase.
#' @noRd
.altitudePhase <- function(from, to, lat, lon) {
  #An instant is an instant whatever zone it is written in, and getSunlightPosition()
  #warns about one that carries no zone of its own, so it is given UTC.
  middle <- as.numeric(from) + as.numeric(difftime(to, from, units="secs")) / 2
  middle <- as.POSIXct(middle, tz="UTC", origin="1970-01-01")
  altitude <- getSunlightPosition(date=middle, lat=lat, lon=lon,
                                  keep="altitude")$altitude * 180 / pi
  #The altitude in degrees at which each band begins, highest first.
  bands <- c(Day=0, Dusk.Civil=-6, Dusk.Naut=-12, Dusk.Astro=-18)
  phase <- names(bands)[altitude >= bands]
  if (length(phase) == 0) {
    return("Night")
  }
  return(phase[[1]])
}

#' Moonlight for a run of days
#'
#' One interval for each day, from the moon's rise on that day to the set that ends
#' it. getMoonTimes() reports the rise and set falling within the calendar day, so
#' on most days the moon sets before it rises, and on some it does not set at all;
#' either way the set that ends the night's moonlight belongs to a later day. Only
#' the next day used to be read, so where the moon stays up for longer than that,
#' as it does for days on end at high latitudes, the set was NA and the interval
#' was dropped without a word. The days that follow are now searched until the set
#' is found.
#'
#' @param dates A vector of consecutive dates
#' @param lat Latitude
#' @param lon Longitude
#' @param tz Time-zone
#' @return A data frame with a row for each date, Start giving the moon's rise on
#'   that day and End the set that ends it, each NA where the moon does not rise or
#'   does not set within the days searched.
#' @noRd
.moonPhases <- function(dates, lat, lon, tz) {
  dates <- as.Date(dates)
  #A fortnight is as long as the moon stays up anywhere, and as above there is
  #nothing to be gained by trying a span in between.
  for (span in c(1, 16)) {
    mt <- .moonTimes(seq(min(dates), max(dates) + span, by=1), lat=lat, lon=lon, tz=tz)
    rises <- as.numeric(mt$rise[match(dates, as.Date(mt$date))])
    sets <- sort(as.numeric(mt$set[!is.na(mt$set)]))
    ends <- vapply(rises, function(rise) {
      later <- sets[sets > rise]
      if (length(later) == 0) NA_real_ else later[[1]]
    }, numeric(1))
    if (!anyNA(ends[!is.na(rises)])) {
      break
    }
  }
  return(data.frame(
    Start = as.POSIXct(rises, tz=tz, origin="1970-01-01"),
    End = as.POSIXct(ends, tz=tz, origin="1970-01-01"),
    row.names = make.unique(rep("Moon", length(dates)), sep="")
  ))
}

#' The first instant of a local day
#'
#' as.POSIXct() of a midnight the local clock skips, as it does where daylight
#' saving begins at midnight, is NA. Noon exists on every day, so the day is
#' anchored there and twelve hours are taken off in real time, which lands on the
#' first instant the clock did reach that day.
#'
#' @param date A date
#' @param tz Time-zone
#' @return The first instant of the day, as POSIXct.
#' @noRd
.startOfDay <- function(date, tz) {
  return(as.POSIXct(paste(as.Date(date), "12:00:00"), tz=tz) - 43200)
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

#' The phase each solar event begins
#'
#' In the order the events occur, named by the suncalc column each is read from.
#' The one source of the correspondence between the two, so that a phase cannot be
#' named for one event in one place and another elsewhere.
#'
#' @return A named character vector, suncalc event name to phase name.
#' @noRd
.solarEventPhases <- function() {
  return(c(
    nightEnd = "Dawn.Astro", nauticalDawn = "Dawn.Naut", dawn = "Dawn.Civil",
    sunrise = "Sunrise", sunriseEnd = "Day",
    sunsetStart = "Sunset", sunset = "Dusk.Civil", dusk = "Dusk.Naut",
    nauticalDusk = "Dusk.Astro", night = "Night"
  ))
}

#' Names of the phases of the day
#'
#' In the order the phases occur, each named for the solar event that begins it.
#'
#' @return A character vector of phase names.
#' @noRd
.dayPhaseNames <- function() {
  return(unname(.solarEventPhases()))
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
    .solarEventPhases()
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
  #As in dayPhases(), the day is the one where the recorder stands.
  centre <- as.Date(date, tz=tz)
  dates <- seq.Date(centre - halfwidth, centre + halfwidth, by = 1)

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
