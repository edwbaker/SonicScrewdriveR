# The field numbers and their meanings are those of the reference
# implementation of the conversion from this format to GUANO, which was written
# against the format as Wildlife Acoustics devices write it.
test_that("The fields of a wamd chunk are read by their numbers", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(0, mdU16(2)),
    mdWamdField(1, "SM3"),
    mdWamdField(2, "SM300123"),
    mdWamdField(3, "2.2.1"),
    mdWamdField(4, "PREFIX"),
    mdWamdField(15, mdU16(10)),
    mdWamdField(18, "SMM-A2"),
    mdWamdField(19, "-27 dBFS")
  )), mdData()))
  w <- wamd(f)

  expect_s3_class(w, "wamd")
  expect_identical(w[["version"]], 2L)
  expect_identical(w[["model"]], "SM3")
  expect_identical(w[["serial"]], "SM300123")
  expect_identical(w[["firmware"]], "2.2.1")
  expect_identical(w[["prefix"]], "PREFIX")
  expect_identical(w[["time_expansion"]], 10L)
  expect_identical(w[["microphone"]], "SMM-A2")
  expect_identical(w[["sensitivity"]], "-27 dBFS")
})

test_that("A field with no name of its own is kept under its number", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(1, "SM4"),
    mdWamdField(21, "something this package has never seen")
  )), mdData()))

  expect_identical(wamd(f)[["wamd:21"]], "something this package has never seen")
})

# The binary fields are an embedded voice recording, the program the device was
# running and the state it was left in. Read as text they give a run of
# punctuation that says nothing about the recording.
test_that("The binary fields are passed over, and their sizes recorded", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(1, "SM3"),
    mdWamdField(16, as.raw(c(1, 2, 3, 0, 255))),
    mdWamdField(17, as.raw(rep(255, 20)))
  )), mdData()))
  w <- wamd(f)

  expect_identical(names(w), c("model"))
  expect_identical(attr(w, "binary")[["program"]], 5)
  expect_identical(attr(w, "binary")[["runstate"]], 20)
})

# The Song Meter Mini Bat writes fields 34 and 35, which nothing published
# names: 128 bytes each, beginning as these do, with the rest zeros. Read as
# text, 34 gave its serial number among a run of control characters.
test_that("The two unnamed binary fields of a Song Meter Mini Bat are passed over", {
  field34 <- c(
    as.raw(c(0, 0, 1, 1, 1, 1)), charToRaw("2MU01960"),
    as.raw(c(0, 0, 0, 0, 2, 0, 0, 0, 0x9b, 0x5b, 0x44, 0, 0x6a, 0xdd, 0, 0))
  )
  field35 <- as.raw(c(0, 0, 1, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x1e, 0x10, 0x1e, 0x28))
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(0, mdU16(1)),
    mdWamdField(34, c(field34, raw(128 - length(field34)))),
    mdWamdField(35, c(field35, raw(128 - length(field35))))
  )), mdData()))
  w <- wamd(f)

  expect_identical(names(w), "version")
  expect_identical(w[["version"]], 1L)
  expect_identical(attr(w, "binary")[["wamd:34"]], 128)
  expect_identical(attr(w, "binary")[["wamd:35"]], 128)
})

test_that("A timestamp is read with the offset from UTC the device was set to", {
  withUtc <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(5, "2015-10-28 20:21:13-04:00")
  )), mdData()))
  t <- wamd(withUtc)[["timestamp"]]

  expect_s3_class(t, "POSIXct")
  expect_identical(format(t, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2015-10-29T00:21:13Z")
})

test_that("A timestamp with no offset is read as a wall clock", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(5, "2015-10-28 20:21:13")
  )), mdData()))
  t <- wamd(f)[["timestamp"]]

  expect_s3_class(t, "POSIXct")
  expect_identical(format(t, "%Y-%m-%d %H:%M:%S"), "2015-10-28 20:21:13")
})

test_that("A time this package cannot read is kept as the text it was", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(5, "sometime last Tuesday")
  )), mdData()))

  expect_warning(w <- wamd(f), "Could not read the wamd timestamp")
  expect_identical(w[["timestamp"]], "sometime last Tuesday")
})

# An offset hour of one digit, as the Song Meter Mini Bat writes in its GUANO,
# was read as no offset at all: the time was taken as a wall clock in the zone
# of the session, which on a machine in London put it an hour out and one in
# Sydney ten hours, with nothing to say so.
for (tz in sessionTimezones) {
  test_that(paste0("A timestamp whose offset hour has one digit keeps its offset in session timezone ", tz), {
    withSessionTimezone(tz, {
      for (case in list(
        c("2025-09-15 21:31:57+2:00", "2025-09-15T19:31:57Z"),
        c("2025-09-15 21:31:57-5:30", "2025-09-16T03:01:57Z"),
        c("2025-09-15T21:31:57.25+9:30", "2025-09-15T12:01:57Z"),
        c("2025-09-15 9:31:57+2:00", "2025-09-15T07:31:57Z")
      )) {
        f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
          mdWamdField(5, case[1])
        )), mdData()))
        expect_silent(t <- wamd(f)[["timestamp"]])

        expect_s3_class(t, "POSIXct")
        expect_identical(format(t, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), case[2])
      }
    })
  })
}

# as.POSIXct() reads a value that merely begins with a time, so whatever came
# after the time, an offset it could not read included, was dropped and the
# time read as a wall clock.
test_that("A timestamp followed by anything else is kept as the text it was", {
  for (text in c(
    "2015-10-28 20:21:13 junk",
    "2015-10-28 20:21:13-04:00 junk",
    "2015-10-28 20:21:13-04"
  )) {
    f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
      mdWamdField(5, text)
    )), mdData()))

    expect_warning(w <- wamd(f), "Could not read the wamd timestamp")
    expect_identical(w[["timestamp"]], text)
  }
})

# No device is known to write a timestamp that stops at the minute, but one can
# mean nothing else, so it is read as readMetadata() reads it rather than kept
# as text with a warning that it could not be read.
wamdTimestampOf <- function(text) {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(5, text)
  )), mdData()))
  wamd(f)[["timestamp"]]
}

for (tz in sessionTimezones) {
  test_that(paste0("A timestamp that stops at the minute is read in session timezone ", tz), {
    withSessionTimezone(tz, {
      for (case in list(
        c("2025-09-15 21:31+2:00", "2025-09-15T19:31:00Z"),
        c("2025-09-15T21:31-05:30", "2025-09-16T03:01:00Z"),
        c("2025-09-15 21:31-0530", "2025-09-16T03:01:00Z"),
        c("2025-09-15 21:31 +02:00", "2025-09-15T19:31:00Z"),
        c("2025-09-15T21:31Z", "2025-09-15T21:31:00Z"),
        c("2025-9-5 9:31+2:00", "2025-09-05T07:31:00Z")
      )) {
        expect_silent(t <- wamdTimestampOf(case[1]))

        expect_s3_class(t, "POSIXct")
        expect_identical(format(t, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), case[2])
      }

      # With no offset it is a wall clock, as a time to the second is.
      expect_silent(minute <- wamdTimestampOf("2025-09-15 21:31"))
      expect_equal(
        as.numeric(minute), as.numeric(wamdTimestampOf("2025-09-15 21:31:00")), tolerance=0
      )
      expect_identical(format(minute, "%Y-%m-%d %H:%M:%S"), "2025-09-15 21:31:00")
    })
  })
}

test_that("A timestamp to the minute followed by anything else, or out of range, is kept as text", {
  for (text in c(
    "2025-09-15 21:31 junk",
    "2025-09-15 21:31+02",
    "2025-09-15 21:31:",
    "2025-09-15 21",
    "2025-09-15 25:00",
    "2025-09-15 21:60"
  )) {
    expect_warning(t <- wamdTimestampOf(text), "Could not read the wamd timestamp")
    expect_identical(t, text)
  }
})

# R warns of such an offset itself, which said the same thing a second time.
test_that("An offset beyond any zone gives only the warning that the time could not be read", {
  for (text in c("2025-09-15 21:31+2400", "2015-10-28 20:21:13+2400")) {
    warnings <- character(0)
    t <- withCallingHandlers(wamdTimestampOf(text), warning=function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    expect_identical(t, text)
    expect_length(warnings, 1)
    expect_match(warnings, "Could not read the wamd timestamp")
  }
})

test_that("Spaces around a timestamp and before its offset are read past", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(5, " 2015-10-28  20:21:13 -04:00 ")
  )), mdData()))
  t <- wamd(f)[["timestamp"]]

  expect_identical(format(t, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2015-10-29T00:21:13Z")
})

test_that("A position is read as a latitude, a longitude and an altitude", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(6, "WGS84,42.1234N,71.5678W,100m")
  )), mdData()))
  position <- wamd(f)[["gpsfirst"]]

  expect_equal(position, c(latitude=42.1234, longitude=-71.5678, altitude=100))
})

# Some devices write a signed number in place of a hemisphere letter.
test_that("A position written as signed numbers is read the same way", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(6, "WGS84,-33.8688,151.2093")
  )), mdData()))

  expect_equal(wamd(f)[["gpsfirst"]], c(latitude=-33.8688, longitude=151.2093))
})

test_that("A position this package cannot read is kept as the text it was", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(6, "WGS84,somewhere,else")
  )), mdData()))

  expect_warning(w <- wamd(f), "Could not read the wamd position")
  expect_identical(w[["gpsfirst"]], "WGS84,somewhere,else")
})

test_that("A file with no wamd chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(wamd(f), "wamd")
  expect_length(wamd(f), 0)
})

test_that("A chunk cut short is read as far as it goes", {
  whole <- mdWav(mdFmt(), mdData(), mdChunk("wamd", c(
    mdWamdField(1, "SM3"),
    mdWamdField(2, "SM300123")
  )))
  # Enough of the chunk goes to take the serial number with it.
  f <- mdFile(whole[seq_len(length(whole) - 10L)])
  w <- wamd(f)

  expect_identical(w[["model"]], "SM3")
  expect_null(w[["serial"]])
})

test_that("A field declaring more bytes than the chunk holds ends the reading", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(1, "SM3"),
    mdU16(2), mdU32(5000), charToRaw("short")
  )), mdData()))

  expect_identical(names(wamd(f)), "model")
})
