# parseFilename() should return the same instant whatever timezone the machine
# running the analysis is set to, so the tests below that involve a datetime are
# repeated in several session timezones rather than pinning the session to UTC.
sessionTimezones <- c("UTC", "America/New_York", "Australia/Sydney")

withSessionTimezone <- function(tz, code) {
  old <- Sys.getenv("TZ", unset=NA)
  Sys.setenv(TZ=tz)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ=old)
  })
  force(code)
}

# Comparing the epoch seconds rather than the POSIXct objects themselves, as
# expect_equal() on a POSIXct allows a relative tolerance that is larger than a
# second for present-day datetimes.
expect_instant <- function(parsed, epoch) {
  expect_equal(as.numeric(parsed$datetime), as.numeric(epoch))
}

test_that(".detectFormat() correctly identifies formats", {
  expect_equal(
    .detectFormat("5E90A4D4.wav"),
    "AudioMoth HEX"
  )
  expect_equal(
    .detectFormat("20240220_162231.wav"),
    "YYYYMMDD_HHMMSS"
  )
  expect_equal(
    .detectFormat("20240220_162231.mp3"),
    "YYYYMMDD_HHMMSS"
  )

  # Check seewave examples
  expect_equal(
    .detectFormat("MNHN_20141225_234500.wav"),
    "Wildlife Acoustics SM2"
  )
  expect_equal(
    .detectFormat("CNRS_0+1_20130824_153000.wav"),
    "Wildlife Acoustics SM3"
  )
  expect_equal(
    .detectFormat("PARIS_-0-_20150410$195550.wav"),
    "Wildlife Acoustics SM3"
  )
  expect_equal(
    .detectFormat("0123456789.wav"),
    "timestamp"
  )

  expect_null(
    .detectFormat("__!__.mp3")
  )
})

test_that(".knownFileFormats() returns known formats", {
  expect_vector(
    .knownFileFormats(),
    "character"
  )
})

test_that("parseFilename() rejects unknown format", {
  files <- "filename.wav"
  expect_error(parseFilename(files, format="orca"), "Unknown format: orca")
})

test_that("parseFilename() rejects undetectable filenames", {
  expect_error(parseFilename("__!__.mp3"), "Could not determine format of __!__.mp3")
})

for (tz in sessionTimezones) {
  test_that(paste0("parseFilename() works as expected in session timezone ", tz), {
    withSessionTimezone(tz, {
      files <- list(
        "5E90A4D4.wav"
      )
      data <- list(
        list(
          filename="5E90A4D4.wav",
          match="AudioMoth HEX",
          datetime=as.POSIXct("2020-04-10 16:54:44", tz="UTC")
        )
      )
      expect_equal(parseFilename(files, timezone="UTC")[[1]]$datetime, data[[1]]$datetime)

      files <- "5E90A4D4.wav"
      data <- list(
          filename="5E90A4D4.wav",
          match="AudioMoth HEX",
          datetime= as.POSIXct("2020-04-10 16:54:44", tz="UTC")
      )
      expect_equal(parseFilename(files, timezone="UTC"), data)

      files <- "20240220_162231.wav"
      data <- list(
        filename="20240220_162231.wav",
        match="YYYYMMDD_HHMMSS",
        datetime= as.POSIXct("2024-02-20 16:22:31", tz="UTC")
      )
      expect_equal(parseFilename(files, timezone="UTC"), data)
      data <- list(
        filename="20240220_162231.wav",
        match="AudioMoth",
        datetime= as.POSIXct("2024-02-20 16:22:31", tz="UTC")
      )
      expect_equal(parseFilename(files, format="AudioMoth", timezone="UTC"), data)
      expect_equal(parseFilename(files, format="AudioMoth"), data)

      files <- "TEST_20240220_162231.wav"
      data <- list(
        filename="TEST_20240220_162231.wav",
        match="Wildlife Acoustics SM2",
        datetime= as.POSIXct("2024-02-20 16:22:31", tz="UTC"),
        model="SM2/SM4",
        prefix="TEST",
        mic=NA,
        geo=NA
      )
      expect_equal(parseFilename(files, timezone="UTC"), data)

      files <- "1708529883.wav"
      data <- list(
        filename="1708529883.wav",
        match="timestamp",
        # Built from the epoch the filename holds, in the zone asked for. The value
        # here used to be a second out and in whatever zone the machine was in, which
        # went unnoticed because expect_equal() compares times that large by relative
        # tolerance and a second is well inside it.
        datetime= as.POSIXct(1708529883, origin=as.POSIXct("1970-01-01", tz="UTC"), tz="GMT")
      )
      expect_equal(parseFilename(files, timezone="GMT"), data)
      expect_equal(as.numeric(parseFilename(files, timezone="GMT")$datetime), 1708529883)
      # The instant is the same wherever it is read.
      expect_equal(
        as.numeric(parseFilename(files, timezone="America/New_York")$datetime),
        as.numeric(parseFilename(files, timezone="UTC")$datetime)
      )
    })
  })

  test_that(paste0("parseFilename() returns the same instant in session timezone ", tz), {
    withSessionTimezone(tz, {
      # The same wall clock in the filename must give the same instant whichever
      # format it is parsed as, and whatever the session timezone is.
      expect_instant(parseFilename("20240220_162231.wav"), 1708446151)
      expect_instant(parseFilename("20240220_162231.wav", format="YYYYMMDD_HHMMSS"), 1708446151)
      expect_instant(parseFilename("20240220_162231.wav", format="AudioMoth"), 1708446151)
      expect_instant(parseFilename("TEST_20240220_162231.wav"), 1708446151)
      expect_instant(parseFilename("TEST_0+1_20240220_162231.wav"), 1708446151)
      expect_instant(parseFilename("TEST_-0-_20240220$162231.wav"), 1708446151)
      expect_instant(parseFilename("20240220.wav"), 1708387200)

      # Formats holding an instant rather than a wall clock.
      expect_instant(parseFilename("5E90A4D4.wav"), 1586537684)
      expect_instant(parseFilename("1708529883.wav"), 1708529883)

      # A missing timezone is the same as the default.
      expect_instant(parseFilename("20240220_162231.wav", timezone=NULL), 1708446151)
      expect_instant(parseFilename("TEST_20240220_162231.wav", timezone=NULL), 1708446151)
    })
  })

  test_that(paste0("parseFilename() parses the wall clock in timezone= in session timezone ", tz), {
    withSessionTimezone(tz, {
      # timezone= is the timezone the clock of the recorder was set to, so it
      # moves the instant for every format holding a wall clock.
      expect_instant(parseFilename("20240220_162231.wav", timezone="Europe/Paris"), 1708442551)
      expect_instant(parseFilename("20240220_162231.wav", format="AudioMoth", timezone="Europe/Paris"), 1708442551)
      expect_instant(parseFilename("TEST_20240220_162231.wav", timezone="Europe/Paris"), 1708442551)
      expect_instant(parseFilename("TEST_0+1_20240220_162231.wav", timezone="Europe/Paris"), 1708442551)
      expect_instant(parseFilename("20240220.wav", timezone="Europe/Paris"), 1708383600)

      # The instant of a format holding an instant does not move, only the
      # timezone it is displayed in.
      expect_instant(parseFilename("5E90A4D4.wav", timezone="Europe/Paris"), 1586537684)
      expect_instant(parseFilename("1708529883.wav", timezone="Europe/Paris"), 1708529883)
      expect_equal(
        format(parseFilename("5E90A4D4.wav", timezone="Europe/Paris")$datetime, "%Y-%m-%d %H:%M:%S"),
        "2020-04-10 18:54:44"
      )
    })
  })

  test_that(paste0("parseFilename() handles paths in session timezone ", tz), {
    withSessionTimezone(tz, {
      parsed <- parseFilename("recordings/site_a/TEST_20240220_162231.wav")
      expect_instant(parsed, 1708446151)
      expect_equal(parsed$prefix, "TEST")
      expect_instant(parseFilename("recordings/site_a/20240220_162231.wav"), 1708446151)
    })
  })
}

test_that("parseFilename() accepts a list of filenames with a forced format", {
  files <- list(
    "20240220_162231.wav",
    "20240221_162231.wav"
  )
  parsed <- parseFilename(files, format="YYYYMMDD_HHMMSS", timezone="UTC")
  expect_length(parsed, 2)
  expect_equal(vapply(parsed, function(x) x$match, character(1)),
               c("YYYYMMDD_HHMMSS", "YYYYMMDD_HHMMSS"))
  expect_instant(parsed[[1]], 1708446151)
  expect_instant(parsed[[2]], 1708532551)

  parsed <- parseFilename(files, timezone="UTC")
  expect_length(parsed, 2)
  expect_instant(parsed[[1]], 1708446151)
})

test_that("match parameter to parseFilename() works as expected", {
  files <- list(
    "20220430.wav",
    "20240121.wav"
  )
  data <- list(
    list(
      filename="20220430.wav",
      match="YYYYMMDD",
      datetime=as.POSIXct("2022-04-30", tz="UTC")
    ),
    list(
      filename="20240121.wav",
      match="YYYYMMDD",
      datetime=as.POSIXct("2024-01-21", tz="UTC")
    )
  )
  expect_equal(parseFilename(files, format="match", timezone="UTC"), data)

  files <- list(
    "20220430.wav",
    "5E90A4D4.wav"
  )
  data <- list(
    list(
      filename="20220430.wav",
      match="AudioMoth HEX",
      datetime=as.POSIXct("1987-01-31 14:03:28", tz="UTC")
    ),
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime=as.POSIXct("2020-04-10 16:54:44", tz="UTC")
    )
  )
  expect_equal(parseFilename(files, format="match", timezone="UTC"), data)
})

test_that("parseFilename() reports when no format matches all filenames", {
  files <- list(
    "20240220_162231.wav",
    "TEST_20240220_162231.wav"
  )
  expect_error(
    parseFilename(files, format="match"),
    "Could not find a single format matching all filenames"
  )
})

test_that("match parameter to parseFilename() detects the format of a single file", {
  expect_equal(parseFilename("20240220_162231.wav", format="match")$match, "YYYYMMDD_HHMMSS")
})
