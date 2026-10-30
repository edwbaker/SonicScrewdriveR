
# Sussex coast, the package defaults.
LAT <- 50.1
LON <- 1.83

test_that("dayPhases returns the phases of one day", {
  d <- dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="UTC")
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 11)
  expect_equal(rownames(d), c("Dawn.Astro", "Dawn.Naut", "Dawn.Civil", "Sunrise",
                              "Day", "Sunset", "Dusk.Civil", "Dusk.Naut",
                              "Dusk.Astro", "Night", "Moon"))
  expect_equal(colnames(d), c("Start", "End"))
  expect_s3_class(d$Start, "POSIXct")
  expect_s3_class(d$End, "POSIXct")
})

test_that("dayPhases gives contiguous phases in order", {
  d <- dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="UTC")
  solar <- d[1:10,]
  expect_false(anyNA(solar$Start))
  expect_true(all(diff(as.numeric(solar$Start)) > 0))
  # Each phase ends where the next begins, and the night runs into the next day.
  # tolerance=0 because the tolerance expect_equal() allows is relative, and at
  # epoch scale it comes to about 25 seconds, which would hide a gap between one
  # phase and the next.
  expect_equal(as.numeric(solar$End[1:9]), as.numeric(solar$Start[2:10]), tolerance=0)
  expect_gt(as.numeric(solar$End[10]), as.numeric(solar$Start[10]))
})

test_that("dayPhases puts the moon in an attribute, not in the phases", {
  d <- dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="UTC")
  moon <- attr(d, "moon")
  expect_named(moon, c("fraction", "phase", "angle", "alwaysUp", "alwaysDown"))
  expect_true(moon$fraction >= 0 && moon$fraction <= 1)
  expect_type(moon$alwaysUp, "logical")
  # The moon row is a real interval, not a set that precedes its own rise.
  expect_lt(as.numeric(d["Moon", "Start"]), as.numeric(d["Moon", "End"]))
})

test_that("dayPhases leaves phases that do not occur as NA", {
  # At 50 N in midsummer the sun never reaches astronomical twilight.
  d <- dayPhases(as.Date("2024-06-15"), lat=LAT, lon=LON, tz="UTC")
  expect_true(is.na(d["Night", "Start"]))
  expect_false(is.na(d["Day", "Start"]))
})

test_that("dayPhases runs a phase on to the next event that occurs", {
  # Night does not start at 50 N in midsummer, so Dusk.Astro does not end when it
  # would have: it runs through to the next morning's nauticalDawn. Taking the end
  # from the next event in the day's own table left it NA.
  d <- dayPhases(as.Date("2024-06-15"), lat=LAT, lon=LON, tz="UTC")
  nextDay <- suncalc::getSunlightTimes(as.Date("2024-06-16"), lat=LAT, lon=LON, tz="UTC")
  expect_false(is.na(d["Dusk.Astro", "End"]))
  expect_equal(as.numeric(d["Dusk.Astro", "End"]), as.numeric(nextDay$nauticalDawn),
               tolerance=0)
})

test_that("dayPhases gives no phase for a day on which no event occurs", {
  # Polar day at Tromso: every event is missing, so no phase begins on this day and
  # the one in effect began in May. dayPhase() reports that phase.
  d <- dayPhases(as.Date("2024-06-20"), lat=69.65, lon=18.96, tz="Europe/Oslo")
  expect_equal(nrow(d), 11)
  expect_true(all(is.na(d$Start[1:10])))
})

test_that("dayPhases finds the moon's set beyond the following day", {
  # At 65 N the moon rises on 11 April and stays up until it sets on the 17th, so
  # the set was NA on both of the two days read and the interval was dropped.
  d <- dayPhases(as.Date("2024-04-11"), lat=65, lon=15, tz="UTC")
  expect_equal(as.Date(d["Moon", "Start"]), as.Date("2024-04-11"))
  expect_equal(as.Date(d["Moon", "End"]), as.Date("2024-04-17"))
})

test_that("dayPhase covers a recording within a single day", {
  r <- dayPhase(time=as.POSIXct("2024-12-15 12:00:00", tz="UTC"), duration=3600,
                lat=LAT, lon=LON, tz="UTC")
  expect_s3_class(r, "data.frame")
  expect_true(all(c("Start", "End", "Start.relative", "End.relative") %in% colnames(r)))
  expect_gt(nrow(r), 0)
  # Midday in December is squarely in the Day phase.
  expect_true("Day" %in% sub("[0-9]+$", "", rownames(r)))
})

test_that("dayPhase spans midnight without looping forever", {
  # Previously the loop refetched the same day, so it never terminated.
  r <- dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=40000,
                lat=LAT, lon=LON, tz="UTC")
  expect_gt(nrow(r), 1)
  expect_gt(length(unique(as.Date(r$Start))), 1)
})

test_that("dayPhase includes a phase the recording ends inside", {
  # Sunrise on this day runs 03:43:44 to 03:48:00. A recording starting before it
  # and ending within it overlaps, but matched none of the three cases previously
  # tested and so was dropped.
  r <- dayPhase(time=as.POSIXct("2024-06-15 03:40:00", tz="UTC"), duration=300,
                lat=LAT, lon=LON, tz="UTC")
  expect_true("Sunrise" %in% sub("[0-9]+$", "", rownames(r)))
})

test_that("dayPhase relative times stay within the recording", {
  duration <- 40000
  r <- dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=duration,
                lat=LAT, lon=LON, tz="UTC")
  expect_true(all(r$Start.relative >= 0))
  expect_true(all(r$End.relative <= duration))
  expect_true(all(r$End.relative >= r$Start.relative))
  # The clamp used to compare relative seconds against an absolute time, so it
  # never fired and the last phase ran past the end of the recording.
  expect_equal(max(r$End.relative), duration)
  expect_equal(min(r$Start.relative), 0)
})

# The seconds of a recording that the solar phases account for. The phases partition
# the day, so a recording is in one of them at every moment and they should account
# for all of it.
solarCoverage <- function(r) {
  solar <- r[!grepl("^Moon", rownames(r)), , drop=FALSE]
  return(sum(solar$End.relative - solar$Start.relative))
}

test_that("dayPhase accounts for the whole of the documented example", {
  # The example in ?dayPhase. Dusk.Astro, Night and Dawn.Astro were all bounded by
  # an event that does not occur at 50 N in midsummer, so all three were dropped and
  # 14241 of the 40000 seconds, the whole of the short night, were in no phase.
  duration <- 40000
  r <- dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=duration,
                lat=LAT, lon=LON, tz="UTC")
  expect_equal(solarCoverage(r), duration)
  expect_true("Dusk.Astro" %in% sub("[0-9]+$", "", rownames(r)))
})

test_that("dayPhase gives the phase of a summer night in southern Scandinavia", {
  # At 58 N on this night nauticalDusk and nauticalDawn do not occur either, and the
  # recording, which is in nautical twilight throughout, came back with the moon and
  # no solar phase at all.
  duration <- 3600
  r <- dayPhase(time=as.POSIXct("2024-06-20 00:30:00", tz="Europe/Stockholm"),
                duration=duration, lat=58, lon=12, tz="Europe/Stockholm")
  expect_equal(sub("[0-9]+$", "", rownames(r))[1], "Dusk.Naut")
  expect_equal(solarCoverage(r), duration)
})

test_that("dayPhase gives polar day as Day and polar night as Night", {
  # Every event is missing for months on end, which returned no row at all for polar
  # day and the moon alone for polar night. The phase is named for where the sun is.
  day <- dayPhase(time=as.POSIXct("2024-06-20 23:00:00", tz="Europe/Oslo"),
                  duration=3600, lat=69.65, lon=18.96, tz="Europe/Oslo")
  expect_equal(sub("[0-9]+$", "", rownames(day))[1], "Day")
  expect_equal(solarCoverage(day), 3600)

  # At 85 N the sun stays below 18 degrees under the horizon from 10 December to
  # 1 January, so noon on the solstice is in the middle of one Night phase.
  night <- dayPhase(time=as.POSIXct("2024-12-21 12:00:00", tz="UTC"),
                    duration=3600, lat=85, lon=15, tz="UTC")
  expect_equal(sub("[0-9]+$", "", rownames(night))[1], "Night")
  expect_equal(solarCoverage(night), 3600)
})

test_that("dayPhase gives a phase at the pole, where no event ever occurs", {
  # At 90 N the sun circles at the altitude of its own declination and suncalc gives
  # every event of every date as NA, so there is no event anywhere to bound a phase
  # with and the sun's altitude is all there is to go on.
  midsummer <- dayPhase(time=as.POSIXct("2024-06-21 12:00:00", tz="UTC"),
                        duration=3600, lat=90, lon=0, tz="UTC")
  expect_equal(rownames(midsummer)[1], "Day")
  expect_equal(solarCoverage(midsummer), 3600)

  midwinter <- dayPhase(time=as.POSIXct("2024-12-21 12:00:00", tz="UTC"),
                        duration=3600, lat=90, lon=0, tz="UTC")
  expect_equal(rownames(midwinter)[1], "Night")
  expect_equal(solarCoverage(midwinter), 3600)
})

test_that("dayPhase names a phase the search cannot bound", {
  # At 89.9 N the sun crosses the thresholds only in the few days around each
  # equinox, which for a handful of dates leaves the 200 days searched holding an
  # event on one side of the recording and none on the other. The phase that cannot
  # be bounded reaches the end of the recording, or back to the start of it, and is
  # named from the sun's altitude. The recording is still accounted for in full.
  start <- as.POSIXct("2024-10-07 20:00:00", tz="UTC")
  unbegun <- dayPhase(time=start, duration=40000, lat=89.9, lon=15, tz="UTC")
  expect_equal(solarCoverage(unbegun), 40000)
  expect_equal(sub("[0-9]+$", "", rownames(unbegun))[1], "Dusk.Civil")
  expect_equal(as.numeric(unbegun$Start[1]), as.numeric(start), tolerance=0)

  start <- as.POSIXct("2024-03-19 20:00:00", tz="UTC")
  unended <- dayPhase(time=start, duration=40000, lat=89.9, lon=15, tz="UTC")
  expect_equal(solarCoverage(unended), 40000)
  expect_equal(as.numeric(unended$End[1]), as.numeric(start) + 40000, tolerance=0)
})

test_that("dayPhase accounts for the whole of a recording wherever it was made", {
  # The phases partition the day at every latitude, so there is no date or place at
  # which part of a recording belongs to no phase.
  duration <- 40000
  dates <- seq.Date(as.Date("2024-01-05"), as.Date("2024-12-31"), by=45)
  for (lat in c(LAT, 69.65, 85)) {
    coverage <- vapply(dates, function(d) {
      r <- dayPhase(time=as.POSIXct(paste(d, "20:00:00"), tz="UTC"), duration=duration,
                    lat=lat, lon=15, tz="UTC")
      solarCoverage(r)
    }, numeric(1))
    expect_equal(coverage, rep(duration, length(dates)),
                 info=paste("latitude", lat))
  }
})

test_that("dayPhase gives contiguous solar phases", {
  r <- dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=40000,
                lat=LAT, lon=LON, tz="UTC")
  solar <- r[!grepl("^Moon", rownames(r)), , drop=FALSE]
  expect_equal(solar$End.relative[-nrow(solar)], solar$Start.relative[-1])
  expect_equal(as.numeric(solar$End[-nrow(solar)]), as.numeric(solar$Start[-1]),
               tolerance=0)
})

test_that("dayPhase finds moonlight that began days before the recording", {
  # At 65 N the moon rose on 11 April and did not set until the 17th, so a recording
  # made between the two is in moonlight that began well outside the days the phases
  # of a recording are otherwise gathered from.
  r <- dayPhase(time=as.POSIXct("2024-04-14 02:00:00", tz="UTC"), duration=3600,
                lat=65, lon=15, tz="UTC")
  expect_true("Moon" %in% rownames(r))
  expect_equal(as.Date(r["Moon", "Start"]), as.Date("2024-04-11"))
})

test_that("dayPhase numbers repeated phases among the rows it returns", {
  # The phases are found by searching outwards from the recording, which at high
  # latitudes runs over hundreds of days, and the numbering used to count those.
  r <- dayPhase(time=as.POSIXct("2024-06-20 23:00:00", tz="Europe/Oslo"),
                duration=3600, lat=69.65, lon=18.96, tz="Europe/Oslo")
  expect_equal(rownames(r), "Day")

  twoDays <- dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=40000,
                      lat=LAT, lon=LON, tz="UTC")
  expect_equal(sum(grepl("^Day", rownames(twoDays))), 2)
  expect_true(all(c("Day", "Day1") %in% rownames(twoDays)))
})

test_that("dayPhases works on every day of the year", {
  # suncalc::getMoonTimes() errors for a single date on which the moon does not both
  # rise and set, which was around one day in fifteen.
  dates <- seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by=1)
  rows <- vapply(dates, function(d) nrow(dayPhases(d, lat=LAT, lon=LON, tz="UTC")), numeric(1))
  expect_true(all(rows == 11))
})

test_that("dayPhases does not warn", {
  # suncalc warns about a type coercion of its own on the same days.
  expect_silent(dayPhases(as.Date("2024-06-12"), lat=LAT, lon=LON, tz="UTC"))
})

test_that("dayPhases carries the moon over midnight", {
  # On this day the moon rises but does not set before midnight, so the set that
  # ends its interval belongs to the following day.
  d <- dayPhases(as.Date("2024-06-12"), lat=LAT, lon=LON, tz="UTC")
  expect_equal(as.Date(d["Moon", "Start"]), as.Date("2024-06-12"))
  expect_equal(as.Date(d["Moon", "End"]), as.Date("2024-06-13"))
})

test_that("daysPhases uses the date it is given", {
  d <- daysPhases(date=as.Date("2020-01-15"), period="month", lat=LAT, lon=LON, tz="UTC")
  expect_equal(range(d$Date), as.Date(c("2019-12-31", "2020-01-30")))
})

test_that("daysPhases returns the right span for each period", {
  m <- daysPhases(date=as.Date("2024-06-15"), period="month", lat=LAT, lon=LON, tz="UTC")
  y <- daysPhases(date=as.Date("2024-06-15"), period="year", lat=LAT, lon=LON, tz="UTC")
  expect_equal(nrow(m), 31)
  expect_equal(nrow(y), 361)
  expect_equal(ncol(m), ncol(y))
})

test_that("daysPhases names every column once", {
  d <- daysPhases(date=as.Date("2024-06-15"), period="month", lat=LAT, lon=LON, tz="UTC")
  expect_false(any(duplicated(names(d))))
  # Dusk.Civil appeared twice and Dusk.Astro not at all.
  expect_true(all(c("Dawn.Astro", "Dawn.Naut", "Dawn.Civil", "Sunrise", "Day",
                    "Sunset", "Dusk.Civil", "Dusk.Naut", "Dusk.Astro", "Night")
                  %in% names(d)))
})

test_that("daysPhases labels columns with the right solar event", {
  date <- as.Date("2024-06-15")
  d <- daysPhases(date=date, period="month", lat=LAT, lon=LON, tz="UTC")
  sc <- suncalc::getSunlightTimes(date, lat=LAT, lon=LON, tz="UTC")
  row <- d[d$Date == date, ]
  expect_equal(row$Sunrise, sc$sunrise)
  expect_equal(row$Day, sc$sunriseEnd)
  expect_equal(row$Sunset, sc$sunsetStart)
  expect_equal(row$Dusk.Civil, sc$sunset)
  expect_equal(row$Dusk.Naut, sc$dusk)
  expect_equal(row$Dusk.Astro, sc$nauticalDusk)
  expect_equal(row$Dawn.Astro, sc$nightEnd)
})

test_that("daysPhases rejects an unknown period", {
  # Previously ret was never assigned and the error named ret rather than period.
  expect_error(daysPhases(period="dog"), "Unknown period for daysPhases: dog")
})

test_that("dayPhases uses the day of the timezone it is given, not of UTC", {
  # 2024-12-15 08:00 in Sydney is 2024-12-14 21:00 UTC, so the two zones
  # disagree about the date. as.Date() reads a POSIXct in UTC unless told
  # otherwise, so the phases returned were the previous day's.
  syd <- as.POSIXct("2024-12-15 08:00:00", tz="Australia/Sydney")
  fromTime <- dayPhases(syd, lat=LAT, lon=LON, tz="Australia/Sydney")
  fromDate <- dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="Australia/Sydney")
  expect_equal(as.numeric(fromTime$Start), as.numeric(fromDate$Start), tolerance=0)

  # The same the other way round: 2024-12-15 20:00 in New York is already
  # 2024-12-16 in UTC.
  ny <- as.POSIXct("2024-12-15 20:00:00", tz="America/New_York")
  expect_equal(
    as.numeric(dayPhases(ny, lat=LAT, lon=LON, tz="America/New_York")$Start),
    as.numeric(dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="America/New_York")$Start),
    tolerance=0
  )
})

for (tz in sessionTimezones) {
  test_that(paste0("dayPhases gives the same phases in session timezone ", tz), {
    withSessionTimezone(tz, {
      d <- dayPhases(as.Date("2024-12-15"), lat=LAT, lon=LON, tz="UTC")
      expect_equal(as.numeric(d$Start[1]), 1734241624, tolerance=0)
      # An instant carrying no timezone of its own.
      fromNaive <- dayPhases(naiveTime(1734300000), lat=LAT, lon=LON, tz="UTC")
      expect_equal(as.numeric(fromNaive$Start), as.numeric(d$Start), tolerance=0)
    })
  })
}

test_that("daysPhases refuses a period it does not know, by name", {
  expect_error(
    daysPhases(as.Date("2024-06-15"), period="fortnight"),
    "Unknown period for daysPhases: fortnight"
  )
})

test_that("daysPhases covers the period asked for", {
  month <- daysPhases(as.Date("2024-06-15"), period="month", lat=LAT, lon=LON)
  year <- daysPhases(as.Date("2024-06-15"), period="year", lat=LAT, lon=LON)
  #Half a period either side of the date given.
  expect_equal(nrow(month), 31)
  expect_equal(nrow(year), 361)
  expect_equal(as.Date(month$Date[1]), as.Date("2024-05-31"))
  expect_equal(as.Date(month$Date[nrow(month)]), as.Date("2024-06-30"))
})

test_that("daysPhases plots the phases of each day without changing them", {
  #Drawing happens on the null device opened for the tests, so nothing is
  #written; what is checked is that the plotting runs and returns the same data.
  drawn <- daysPhases(as.Date("2024-06-15"), period="month", lat=LAT, lon=LON, plot=TRUE)
  quiet <- daysPhases(as.Date("2024-06-15"), period="month", lat=LAT, lon=LON, plot=FALSE)
  expect_equal(drawn, quiet)
})

test_that("daysPhases plots a polar month, where some phases never arrive", {
  #Inside the Arctic circle in June the sun does not set, so most of the phases
  #are NA and the lines drawn have gaps in them.
  expect_no_error(
    daysPhases(as.Date("2024-06-15"), period="month", lat=78, lon=15, plot=TRUE)
  )
})

test_that("dayPhase refuses a time that names no instant", {
  # A Date is a count of days, so time + duration added the duration as days and
  # the answer was hundreds of thousands of rows reaching back into the year
  # before, with only a warning about incompatible methods to show for it.
  expect_error(
    dayPhase(time=as.Date("2024-06-15"), duration=40000, lat=LAT, lon=LON, tz="UTC"),
    "time must be a POSIXct or POSIXlt, not Date"
  )
  expect_error(
    dayPhase(time="2024-06-15 20:00:00", duration=40000, lat=LAT, lon=LON, tz="UTC"),
    "time must be a POSIXct or POSIXlt, not character"
  )
  expect_error(
    dayPhase(time=1718481600, duration=40000, lat=LAT, lon=LON, tz="UTC"),
    "time must be a POSIXct or POSIXlt, not numeric"
  )
})
