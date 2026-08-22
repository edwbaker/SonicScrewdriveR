Sys.setenv(TZ='UTC')

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
  expect_equal(as.numeric(solar$End[1:9]), as.numeric(solar$Start[2:10]))
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
