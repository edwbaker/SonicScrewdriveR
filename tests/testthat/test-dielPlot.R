test_that("dielLabels rejects unknown format", {
  expect_error(dielLabels("neon tetra"), "Unknown format for dielLabels: neon tetra")
})

test_that("dielLabels gives expected output", {
  l <- dielLabels("clock24")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)

  l <- dielLabels("clock12")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)
})

test_that("dielPositions rejects unknown format", {
  expect_error(dielPositions("aardvark"), "Unknown format for dielPositions: aardvark")
})

test_that("dielPositions gives expected output", {
  l <- dielPositions("3hourly")
  expect_true(is.numeric(l))
  expect_equal(length(l), 8)

  l <- dielPositions("hours")
  expect_true(is.numeric(l))
  expect_equal(length(l), 24)

  l <- dielPositions("minutes")
  expect_true(is.numeric(l))
  expect_equal(length(l), 1440)
})

test_that("dielFraction rejects unknown formats", {
  expect_error(dielFraction(2, "wombat"), "Unknown input.unit for dielFraction: wombat")
  expect_error(dielFraction(2, "hours", "worms"), "Unknown output.unit for dielFraction: worms")
})

test_that("dielFraction gives expected output", {
  expect_equal(dielFraction(0, "minutes", "fraction"), 0)
  expect_equal(dielFraction(0, "minutes", "radians"), 0)
  expect_equal(dielFraction(12, "hours", "fraction"), 0.5)
  expect_equal(dielFraction(12, "hours", "radians"), pi)
  # dielPlot() draws its bands in UTC, so a time is placed on the dial by where
  # it falls in UTC. Written as a string, or with its timezone given, noon is
  # half way round wherever the test runs. as.POSIXct() without a timezone reads
  # the string in the session's zone, which is a different moment on each
  # machine, so it is not used here.
  expect_equal(dielFraction("1985-02-11 12:00", "POSIX", "fraction"), 0.5)
  expect_equal(dielFraction(as.POSIXct("1985-02-11 12:00", tz="UTC"), "POSIX", "fraction"), 0.5)
  expect_equal(dielFraction(24, "hours", "fraction"), 1)
  expect_equal(dielFraction(24, "hours", "radians"), 2*pi)
})

test_that("tzRot gives expected output", {
  expect_equal(tzRot(12), 0)
  expect_equal(tzRot(0, init=0), 0)
})

test_that("emptyDiel rejects unknown formats", {
  expect_error(emptyDiel(method="horse"), "Unknown method for emptyDiel: horse")
})

test_that("emptyDiel gives no warnings", {
  expect_silent(emptyDiel())
  expect_silent(emptyDiel(rot=0))
})

test_that("dielPlot gives no warnings", {
  expect_silent(dielPlot("2024-12-21", lat=54, lon=66))
  expect_silent(dielPlot("2024-06-21", lat=54, lon=66))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, rot="Solar Noon"))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, plot=c("Solar Noon", "Nadir")))
  expect_silent(dielPlot("2024-02-12", lat=54, lon=0, legend=TRUE))
})

test_that("dielRings gives no warnings", {
  expect_silent(dielPlot(Sys.time(), lat=54, lon=0))
  expect_silent(dielRings("tom", "0000", "1200"))
})

test_that("dielPlot draws the date it is given whatever the session timezone", {
  # The instants were built at local midnight and then relabelled as UTC, so a
  # session east of UTC drew the previous day.
  pdf(NULL)
  on.exit({dev.off(); Sys.setenv(TZ="UTC")})
  for (tz in c("UTC", "Australia/Sydney", "America/Los_Angeles")) {
    Sys.setenv(TZ=tz)
    expect_silent(dielPlot("2024-06-21", lat=54, lon=0))
  }
})

for (tz in sessionTimezones) {
  test_that(paste0("dielFraction places an instant the same way in session timezone ", tz), {
    withSessionTimezone(tz, {
      # 2024-12-16 03:20:00 UTC as an instant carrying no timezone of its own,
      # which used to be read in the session's zone and so moved around the dial
      # with the machine.
      expect_equal(dielFraction(naiveTime(1734319200), output.unit="fraction"), 12000/86400)
      # A time carrying a timezone is read in it, whatever the session is.
      expect_equal(
        dielFraction(as.POSIXct("2024-12-16 03:20:00", tz="Australia/Sydney"), output.unit="fraction"),
        12000/86400
      )
      # Naming a timezone overrides the one the time carries.
      expect_equal(
        dielFraction(as.POSIXct("2024-12-16 03:20:00", tz="UTC"), output.unit="fraction",
                     timezone="Australia/Sydney"),
        51600/86400
      )
      # suncalc labels its output UTC, so the bands of a plot are unaffected.
      tim <- suncalc::getSunlightTimes(as.Date("2024-06-21"), lat=54, lon=0, tz="UTC")
      expect_equal(dielFraction(tim$solarNoon, output.unit="fraction"), 0.5021528, tolerance=1e-6)
    })
  })
}

#The bands are drawn by radialPolygon(), so recording its calls says which bands
#were drawn and how far round the dial each one reached.
polygonsDrawn <- function(expr, env=parent.frame()) {
  drawn <- list()
  testthat::local_mocked_bindings(
    radialPolygon = function(angle1, angle2, radius1, radius2, ...) {
      drawn[[length(drawn)+1]] <<- list(from=angle1, to=angle2)
      invisible(NULL)
    },
    .env = env
  )
  force(expr)
  return(drawn)
}

#The sun's altitude through the day is drawn first, as one polygon of a thousand
#angles. The bands that follow are each a single arc.
bandsDrawn <- function(drawn) {
  Filter(function(p) length(p$to) == 1, drawn)
}

wholeCircleDrawn <- function(drawn) {
  any(vapply(drawn, function(p) {
    isTRUE(all.equal(p$from, 0)) && isTRUE(all.equal(p$to, 2*pi))
  }, logical(1)))
}

test_that("plotting with plotrix says so when plotrix is missing", {
  local_mocked_bindings(package.installed = function(name, ...) FALSE)
  expect_error(
    dielPlot("2024-06-21", lat=54, lon=0),
    "Plotrix must be installed to plot using Plotrix."
  )
})

test_that("a day on which the sun never rises is drawn as twilight all round", {
  #At 70 degrees north in late December the sun does not rise, so there is no
  #sunrise to end the civil twilight band at and it covers the whole dial.
  drawn <- polygonsDrawn(dielPlot("2024-12-21", lat=70, lon=0, plot="Civil Twilight"))
  expect_gt(length(drawn), 0)
  expect_true(wholeCircleDrawn(drawn))
})

test_that("a day with no astronomical twilight at all is drawn as night all round", {
  #At 85 degrees north the sun stays more than 18 degrees below the horizon, so
  #astronomical night neither begins nor ends and covers the whole dial.
  drawn <- polygonsDrawn(dielPlot("2024-12-21", lat=85, lon=0, plot="Night"))
  expect_gt(length(drawn), 0)
  expect_true(wholeCircleDrawn(drawn))
})

test_that("a day whose twilight does end is not drawn as twilight all round", {
  drawn <- polygonsDrawn(dielPlot("2024-06-21", lat=54, lon=0, plot="Civil Twilight"))
  expect_gt(length(drawn), 0)
  expect_false(wholeCircleDrawn(drawn))
})

test_that("a sunrise that never completes is drawn as far as solar noon", {
  #At 67 degrees north in late December the top of the sun clears the horizon
  #but the whole disc does not, so there is no sunriseEnd to draw to.
  tim <- suncalc::getSunlightTimes(date=as.Date("2024-12-21"), lat=67, lon=0, tz="UTC")
  expect_false(is.na(tim$sunrise))
  expect_true(is.na(tim$sunriseEnd))

  bands <- bandsDrawn(polygonsDrawn(dielPlot("2024-12-21", lat=67, lon=0, plot="Sunrise")))
  expect_length(bands, 1)
  expect_equal(bands[[1]]$from, dielFraction(tim$sunrise))
  expect_equal(bands[[1]]$to, dielFraction(tim$solarNoon))
})

test_that("a sunset that never completes is drawn from solar noon", {
  tim <- suncalc::getSunlightTimes(date=as.Date("2024-12-21"), lat=67, lon=0, tz="UTC")
  expect_false(is.na(tim$sunset))
  expect_true(is.na(tim$sunsetStart))

  bands <- bandsDrawn(polygonsDrawn(dielPlot("2024-12-21", lat=67, lon=0, plot="Sunset")))
  expect_length(bands, 1)
  expect_equal(bands[[1]]$from, dielFraction(tim$solarNoon))
  expect_equal(bands[[1]]$to, dielFraction(tim$sunset))
})

test_that("a day on which the sun never rises has no sunrise band", {
  bands <- bandsDrawn(polygonsDrawn(dielPlot("2024-12-21", lat=70, lon=0, plot="Sunrise")))
  expect_length(bands, 0)
})

test_that("a polar day and a polar night are both drawn without complaint", {
  expect_silent(dielPlot("2024-06-21", lat=85, lon=0))
  expect_silent(dielPlot("2024-12-21", lat=85, lon=0))
  expect_silent(dielPlot("2024-12-21", lat=70, lon=0, legend=TRUE))
})
