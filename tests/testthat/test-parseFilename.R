# sessionTimezones, withSessionTimezone() and expect_instant() are in
# helper-timezone.R, as the time tests of several functions use them.

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
  # A name of nothing but digits could as easily be a record number, so the
  # format it is read as carries the others it could have been.
  expect_equal(
    as.character(.detectFormat("0123456789.wav")),
    "timestamp"
  )
  expect_equal(
    attr(.detectFormat("0123456789.wav"), "alternatives"),
    c("timestamp", "record number")
  )
  expect_equal(
    attr(.detectFormat("20240220.wav"), "alternatives"),
    c("YYYYMMDD", "AudioMoth HEX", "record number")
  )
  expect_null(attr(.detectFormat("20240220_162231.wav"), "alternatives"))

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
        datetime= as.POSIXct(1708529883, origin=as.POSIXct("1970-01-01", tz="UTC"), tz="GMT"),
        # Ten digits read as a count of seconds since 1970 read just as well as
        # a record number, so the reading says it is a guess.
        ambiguous=TRUE,
        alternatives=c("timestamp", "record number")
      )
      expect_equal(parseFilename(files, timezone="GMT"), data)
      expect_instant(parseFilename(files, timezone="GMT"), 1708529883)
      # The instant is the same wherever it is read.
      expect_equal(
        as.numeric(parseFilename(files, timezone="America/New_York")$datetime),
        as.numeric(parseFilename(files, timezone="UTC")$datetime),
        tolerance=0
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

test_that("parseFilename() accepts a character vector of filenames", {
  # list.files() returns a character vector, not a list.
  files <- c(
    "20240220_162231.wav",
    "20240221_162231.wav"
  )
  parsed <- parseFilename(files, timezone="UTC")
  expect_length(parsed, 2)
  expect_instant(parsed[[1]], 1708446151)
  expect_instant(parsed[[2]], 1708532551)
  expect_equal(parsed, parseFilename(as.list(files), timezone="UTC"))

  parsed <- parseFilename(files, format="YYYYMMDD_HHMMSS", timezone="UTC")
  expect_equal(vapply(parsed, function(x) x$match, character(1)),
               c("YYYYMMDD_HHMMSS", "YYYYMMDD_HHMMSS"))

  # A single-element vector keeps behaving like a single filename.
  parsed <- parseFilename(c("20240220_162231.wav"), timezone="UTC")
  expect_equal(parsed$match, "YYYYMMDD_HHMMSS")
})

test_that("match parameter to parseFilename() works on a character vector", {
  files <- c(
    "20220430.wav",
    "5E90A4D4.wav"
  )
  parsed <- parseFilename(files, format="match", timezone="UTC")
  expect_equal(parsed, parseFilename(as.list(files), format="match", timezone="UTC"))
  expect_equal(vapply(parsed, function(x) x$match, character(1)),
               c("AudioMoth HEX", "AudioMoth HEX"))
})

test_that("match parameter to parseFilename() works as expected", {
  files <- list(
    "20220430.wav",
    "20240121.wav"
  )
  # Finding a format that fits every name in the list does not settle what
  # these names are: a list of record numbers agrees with itself exactly as
  # readily as a list of dates does, so the reading is still a guess and still
  # says so. A format the caller names is a different matter, and carries no
  # alternatives.
  ambiguity <- list(
    ambiguous=TRUE,
    alternatives=c("YYYYMMDD", "AudioMoth HEX", "record number")
  )
  data <- list(
    c(
      list(
        filename="20220430.wav",
        match="YYYYMMDD",
        datetime=as.POSIXct("2022-04-30", tz="UTC")
      ),
      ambiguity
    ),
    c(
      list(
        filename="20240121.wav",
        match="YYYYMMDD",
        datetime=as.POSIXct("2024-01-21", tz="UTC")
      ),
      ambiguity
    )
  )
  expect_equal(parseFilename(files, format="match", timezone="UTC"), data)
  expect_false("ambiguous" %in% names(
    parseFilename("20220430.wav", format="YYYYMMDD", timezone="UTC")
  ))

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

# A format is matched on the shape of the digits, so a name spelling a day that
# does not exist matched one all the same and strptime()'s NA was reported as
# the datetime. NA is also what a format carrying no time gives, which TSA
# names do, so a caller could not tell a date this could not read from a name
# with no date in it. unparsed is what tells them apart.

test_that("A name whose digits are no real date names datetime unparsed", {
  impossible <- c(
    "20240230_120000.wav",  # 30 February
    "20241301_120000.wav",  # month 13
    "20240101_250000.wav",  # hour 25
    "20240101_006100.wav",  # minute 61
    "20240230.wav"
  )
  for (name in impossible) {
    p <- suppressWarnings(parseFilename(name))
    expect_true(is.na(p$datetime), info=name)
    expect_equal(p$unparsed, "datetime", info=name)
    # The format it matched is still reported: the name does follow that
    # convention, it just does not spell a date.
    expect_false(is.na(p$match), info=name)
  }
})

test_that("A format carrying no time names nothing unparsed", {
  # The documented NA of a TSA name, which is a name with no time in it rather
  # than a time this could not read. The two were the same answer before.
  p <- parseFilename("TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")
  expect_true(is.na(p$datetime))
  expect_null(p$unparsed)
  expect_equal(p$taxon, "Porzana parva")
})

test_that("A format the caller names that the name cannot be in is reported", {
  p <- suppressWarnings(parseFilename("recording.wav", format="YYYYMMDD"))
  expect_true(is.na(p$datetime))
  expect_equal(p$unparsed, "datetime")

  # A timestamp that is not a count of seconds coerced to NA with only an
  # "NAs introduced by coercion" warning, which names neither the file nor the
  # format it was read as.
  p <- suppressWarnings(parseFilename("recording.wav", format="timestamp"))
  expect_true(is.na(p$datetime))
  expect_equal(p$unparsed, "datetime")
})

test_that("The warning names the file and the format, and carries a class", {
  expect_warning(parseFilename("20240230_120000.wav"), "20240230_120000.wav",
                 fixed=TRUE)
  expect_warning(parseFilename("20240230_120000.wav"), "YYYYMMDD_HHMMSS",
                 fixed=TRUE)

  w <- tryCatch(parseFilename("20240230_120000.wav"), warning=function(w) w)
  expect_s3_class(w, "unparsedDatetime")
  expect_s3_class(w, "unparsedField")
  # Classed so that a walk over thousands of files can silence this one kind
  # without silencing the rest, which is what makes warning here affordable.
  expect_silent(withCallingHandlers(
    parseFilename("20240230_120000.wav"),
    unparsedField=function(w) invokeRestart("muffleWarning")
  ))
})

test_that("A name that reads gives no unparsed and no warning", {
  for (name in c("20200609_191730.wav", "20240220.wav", "5E90A4D4.wav",
                 "1591730250.wav")) {
    p <- parseFilename(name)
    expect_null(p$unparsed, info=name)
  }
  expect_silent(parseFilename("20200609_191730.wav"))
})

test_that("A name with an impossible date keeps what else it carries", {
  # A Wildlife Acoustics name gives a model and a prefix beside its date, and
  # those are read from the name whether or not the digits are a real date.
  p <- suppressWarnings(parseFilename("PREFIX_20240230_120000.wav"))
  expect_equal(p$prefix, "PREFIX")
  expect_true(is.na(p$datetime))
  expect_equal(p$unparsed, "datetime")
})

test_that("readMetadata keeps a name's other fields and stays quiet", {
  # .mdFilenameSource() catches warnings and abandons the reading, so the
  # warning is muffled there rather than costing the model and the prefix. No
  # date is taken from such a name in any case.
  # The name has to be exactly this, so it is built rather than drawn from
  # tempfile(), which appends characters of its own and leaves a name that
  # matches no format.
  f <- file.path(tempdir(), "PREFIX_20240230_120000.wav")
  read <- .mdFilenameSource(f)
  expect_false(is.null(read))
  expect_equal(read$prefix, "PREFIX")
  expect_silent(.mdFilenameSource(f))

  # And the record itself mentions no date, as it did before.
  expect_null(.mdNormaliseFilename(read)$datetime)
})
