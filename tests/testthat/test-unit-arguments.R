#' A short multichannel wave, for the functions that take one.
#' @noRd
twoChannelWave <- function(n=4800, samp.rate=48000) {
  set.seed(1)
  data <- cbind(stats::rnorm(n), stats::rnorm(n))
  return(tuneR::WaveMC(data * 1e4, samp.rate=samp.rate, bit=32, pcm=FALSE))
}

test_that("input.unit is accepted wherever a value's unit is given", {
  expect_equal(convert2Pascals(1, input.unit="kPa"), 1000)
  expect_equal(convert2dyne_cm2(1, input.unit="Pa"), 10)
  expect_equal(convert2Kelvin(0, input.unit="C"), 273.15)
  expect_equal(convert2Celsius(0, input.unit="K"), -273.15)
  expect_equal(convert2degrees(pi, input.unit="radians"), 180)
  expect_equal(convert2radians(180, input.unit="degrees"), pi)
  expect_equal(convert2bytes(8, input.unit="bits"), 1)
  expect_equal(convert2seconds(1, input.unit="minutes"), 60)
  expect_equal(humanTime(1, input.unit="hours"), "1 hour")
  expect_equal(region(input.unit="seconds", from=0, to=1)@unit, "seconds")
  expect_equal(dielFraction(0, input.unit="seconds", output.unit="fraction"), 0)
  expect_equal(yearlyFraction(0, input.unit="day", output.unit="fraction"), 0)
})

test_that("output.unit is accepted wherever a result's unit is given", {
  expect_equal(referencePressure(output.unit="dyne_cm2"), 0.0002)
  expect_equal(referenceIntensity(output.unit="watt_cm2"), 10^-16)
  expect_equal(wavelength(1000, speed=343, output.unit="cm"), 34.3)
  expect_equal(dielFraction(0, output.unit="fraction"), 0)
  expect_equal(yearlyFraction(0, input.unit="day", output.unit="radians"), 0)

  mics <- rbind(c(0, 0), c(1, 0), c(0, 1))
  b <- bearing(c(0, -1/343, 0), mics, output.unit="radians")
  expect_equal(b$unit, "radians")
})

test_that("the former argument names still work, with a warning", {
  expect_warning(x <- convert2Pascals(1, input="kPa"), "input argument of convert2Pascals")
  expect_equal(x, 1000)

  expect_warning(x <- convert2dyne_cm2(1, input="Pa"), "input argument of convert2dyne_cm2")
  expect_equal(x, 10)

  expect_warning(x <- convert2Kelvin(0, input="C"), "input argument of convert2Kelvin")
  expect_equal(x, 273.15)

  expect_warning(x <- convert2Celsius(0, input="K"), "input argument of convert2Celsius")
  expect_equal(x, -273.15)

  expect_warning(x <- convert2degrees(pi, input="radians"), "input argument of convert2degrees")
  expect_equal(x, 180)

  expect_warning(x <- convert2radians(180, input="degrees"), "input argument of convert2radians")
  expect_equal(x, pi)

  expect_warning(x <- convert2bytes(8, input="bits"), "input argument of convert2bytes")
  expect_equal(x, 1)

  expect_warning(x <- convert2seconds(1, input="minutes"), "input argument of convert2seconds")
  expect_equal(x, 60)

  expect_warning(x <- humanTime(1, unit="hours"), "unit argument of humanTime")
  expect_equal(x, "1 hour")

  expect_warning(x <- region(unit="seconds", from=0, to=1), "unit argument of region")
  expect_equal(x@unit, "seconds")

  expect_warning(x <- referencePressure(unit="dyne_cm2"), "unit argument of referencePressure")
  expect_equal(x, 0.0002)

  expect_warning(x <- referenceIntensity(unit="watt_cm2"), "unit argument of referenceIntensity")
  expect_equal(x, 10^-16)

  expect_warning(x <- wavelength(1000, speed=343, unit="cm"), "unit argument of wavelength")
  expect_equal(x, 34.3)

  expect_warning(x <- dielFraction(0, input="seconds"), "input argument of dielFraction")
  expect_equal(x, 0)

  expect_warning(
    x <- dielFraction(0, input.unit="seconds", unit="fraction"),
    "unit argument of dielFraction"
  )
  expect_equal(x, 0)

  expect_warning(x <- yearlyFraction(0, input="day"), "input argument of yearlyFraction")
  expect_equal(x, 0)

  expect_warning(
    x <- yearlyFraction(0, input.unit="day", unit="fraction"),
    "unit argument of yearlyFraction"
  )
  expect_equal(x, 0)

  mics <- rbind(c(0, 0), c(1, 0), c(0, 1))
  expect_warning(
    x <- bearing(c(0, -1/343, 0), mics, unit="radians"),
    "unit argument of bearing"
  )
  expect_equal(x$unit, "radians")
})

test_that("the first argument of convert2seconds is t, and T still works", {
  expect_equal(convert2seconds(t=1, input.unit="minutes"), 60)
  expect_warning(x <- convert2seconds(T=1, input.unit="minutes"), "T argument of convert2seconds")
  expect_equal(x, 60)
})

test_that("readAudio and readRespeaker6 take input.unit", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readAudio(f, from=1, to=2, input.unit="seconds")
  expect_true(inherits(w, "Wave"))

  expect_warning(
    w2 <- readAudio(f, from=1, to=2, units="seconds"),
    "units argument of readAudio"
  )
  expect_equal(w2@left, w@left)
})

test_that("tdoa takes input.unit", {
  w <- twoChannelWave()
  d <- tdoa(w, from=0, to=0.05, input.unit="seconds")
  expect_equal(d$channel, 1:2)

  expect_warning(
    d2 <- tdoa(w, from=0, to=0.05, units="seconds"),
    "units argument of tdoa"
  )
  expect_equal(d2$samples, d$samples)
})

test_that("the new and former names give the same answer", {
  expect_equal(
    suppressWarnings(convert2Pascals(10, input="dyne_cm2")),
    convert2Pascals(10, input.unit="dyne_cm2")
  )
  expect_equal(
    suppressWarnings(wavelength(1000, speed=343, unit="cm")),
    wavelength(1000, speed=343, output.unit="cm")
  )
  expect_equal(
    suppressWarnings(humanTime(90, unit="minutes")),
    humanTime(90, input.unit="minutes")
  )
})

test_that("unit arguments can still be given positionally", {
  expect_equal(convert2Pascals(1, "kPa"), 1000)
  expect_equal(convert2Kelvin(0, "C"), 273.15)
  expect_equal(convert2seconds(1, "minutes"), 60)
  expect_equal(region("seconds", 0, 1)@unit, "seconds")
  expect_equal(wavelength(1000, 343, "cm"), 34.3)
})

test_that("the unit arguments that are not units of measurement are unchanged", {
  #normalise() passes unit to tuneR, where it selects a bit depth.
  w <- tuneR::sine(440, duration=480, samp.rate=48000)
  expect_true(inherits(suppressWarnings(normalise(w, unit="16")), "Wave"))

  #humanBytes() takes units to choose between decimal and binary prefixes.
  expect_equal(humanBytes(1024, units="binary"), "1 KiB")
  expect_equal(humanBytes(1000, units="decimal"), "1 kB")
})

#A former name given by keyword no longer occupies the position it used to, so
#a call that mixes it with positional arguments is the case where a shim read
#in the function body bound them one place too early. These calls are written
#as they would have been before the rename, and must give what they gave then.
test_that("a former name mixed with positional arguments binds as it used to", {
  expect_warning(x <- convert2seconds(T=1, "hours"), "T argument of convert2seconds")
  expect_equal(x, 3600)

  expect_warning(x <- convert2seconds(T=90, "seconds"), "T argument of convert2seconds")
  expect_equal(x, 90)

  expect_warning(x <- region(unit="seconds", 0, 10), "unit argument of region")
  expect_equal(x@from, 0)
  expect_equal(x@to, 10)
  expect_equal(x@unit, "seconds")

  #digits is the argument after the renamed one, so it is the one that moved.
  expect_warning(x <- humanTime(95, unit="seconds", 1), "unit argument of humanTime")
  expect_equal(x, humanTime(95, input.unit="seconds", digits=1))

  #output.unit is the argument after the renamed one here.
  expect_warning(
    x <- dielFraction(6, input="hours", "fraction"),
    "input argument of dielFraction"
  )
  expect_equal(x, 0.25)

  expect_warning(
    x <- yearlyFraction(73, 2022, input="day", "fraction"),
    "input argument of yearlyFraction"
  )
  expect_equal(x, 73/365)
})

test_that("a former name mixed with positionals binds as it used to, for waves", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")

  #header is the argument after the renamed one, and selects an entirely
  #different kind of return value.
  expect_warning(
    h <- readRespeaker6(f, 1, 100, units="samples", TRUE),
    "units argument of readRespeaker6"
  )
  expect_true(is.list(h))
  expect_false(inherits(h, "WaveMC"))

  #method is the argument after the renamed one.
  w <- twoChannelWave()
  expect_warning(
    d <- tdoa(w, 1, 0, 0.05, units="seconds", "cc"),
    "units argument of tdoa"
  )
  expect_equal(d, tdoa(w, 1, 0, 0.05, input.unit="seconds", method="cc"))
})

test_that("both former names at once bind as they used to", {
  expect_warning(x <- convert2seconds(T=1, input="hours"))
  expect_equal(x, 3600)

  expect_warning(x <- dielFraction(6, input="hours", unit="fraction"))
  expect_equal(x, 0.25)
})

test_that("a misspelled argument is still an error, not silently ignored", {
  expect_error(convert2Pascals(1, inpt="kPa"))
  expect_error(convert2seconds(1, inpt="hours"))
  expect_error(wavelength(1000, speed=343, untis="cm"))
  expect_error(region("seconds", form=0, to=1))
})

test_that("a partially matched argument name still resolves", {
  expect_equal(convert2seconds(1, input.u="hours"), 3600)
  expect_equal(wavelength(1000, speed=343, output.u="cm"), 34.3)
})

#A former name reaches a function in more ways than being written out in the
#call: abbreviated to a name that matches it and nothing else, or forwarded
#through the dots of a wrapper, which is what lapply() does with its extra
#arguments. Each must be seen, and give what the name in full gives.
test_that("a former name is seen when abbreviated", {
  expect_warning(x <- wavelength(1000, speed=343, uni="cm"), "unit argument of wavelength")
  expect_equal(x, 34.3)

  expect_warning(x <- humanTime(1, uni="hours"), "unit argument of humanTime")
  expect_equal(x, "1 hour")

  expect_warning(x <- referencePressure(uni="dyne_cm2"), "unit argument of referencePressure")
  expect_equal(x, 0.0002)

  expect_warning(x <- region(uni="seconds", from=0, to=10), "unit argument of region")
  expect_equal(c(x@from, x@to), c(0, 10))
  expect_equal(x@unit, "seconds")
})

test_that("a former name is seen when forwarded through dots", {
  through <- function(...) wavelength(1000, speed=343, ...)
  expect_warning(x <- through(unit="cm"), "unit argument of wavelength")
  expect_equal(x, 34.3)

  #The value lives in a frame beyond the one the call is made from, so its
  #promise has to be forced where it belongs rather than re-evaluated.
  outer <- function() {
    u <- "cm"
    through(unit=u)
  }
  expect_warning(x <- outer(), "unit argument of wavelength")
  expect_equal(x, 34.3)

  secs <- function(...) convert2seconds(1, ...)
  expect_warning(x <- secs(input="hours"), "input argument of convert2seconds")
  expect_equal(x, 3600)

  #A former name mixed with a positional argument, both arriving through dots.
  expect_warning(
    x <- (function(...) convert2seconds(...))(T=1, "hours"),
    "T argument of convert2seconds"
  )
  expect_equal(x, 3600)
})

test_that("a former name is seen when passed through lapply", {
  expect_warning(
    x <- lapply(1000, wavelength, speed=343, unit="cm"),
    "unit argument of wavelength"
  )
  expect_equal(x[[1]], 34.3)

  expect_warning(
    x <- lapply(1, convert2seconds, input="hours"),
    "input argument of convert2seconds"
  )
  expect_equal(x[[1]], 3600)
})

test_that("a former name and the argument replacing it cannot both be given", {
  expect_error(
    wavelength(1000, 343, output.unit="m", unit="cm"),
    "same argument"
  )
  expect_error(
    suppressWarnings(dielFraction(6, input.unit="hours", input="minutes")),
    "same argument"
  )
})

test_that("an argument beyond the former signature is still unused", {
  #convert2Pascals(P, input) took two arguments, so a third was an error.
  expect_error(convert2Pascals(1, "kPa", "Pa"), "unused argument")
})

test_that("the new names behave as they did, whichever way they are given", {
  expect_equal(convert2seconds(1, "hours"), 3600)
  expect_equal(convert2seconds(1, input.u="hours"), 3600)
  expect_equal(do.call(convert2seconds, list(1, input.unit="hours")), 3600)
  expect_equal(convert2seconds(c(1, 2), input.unit="hours"), c(3600, 7200))
  expect_equal(convert2seconds(2*3, input.unit="hours"), 21600)
})

test_that("an abbreviation matching both names is ambiguous", {
  #input is a prefix of input.unit, so while both are formals an abbreviation
  #of the former name matches two arguments. R reports it rather than guessing.
  expect_error(convert2seconds(1, inp="hours"), "matches multiple formal arguments")
  expect_error(convert2Pascals(1, inp="kPa"), "matches multiple formal arguments")
})
