Sys.setenv(TZ='UTC')

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

test_that("parseFilename() works as expected", {
  files <- "__!__.mp3"
  expect_error(parseFilename(files), "Could not determine format of __!__.mp3")

  files <- list(
    "5E90A4D4.wav"
  )
  data <- list(
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime=as.POSIXct("2020-04-10 16:54:44 UTC", tz="UTC")
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
    datetime= as.POSIXct("2024-02-20 16:22:31 UTC", tz="UTC")
  )
  expect_equal(parseFilename(files, timezone="UTC"), data)
  data <- list(
    filename="20240220_162231.wav",
    match="AudioMoth",
    datetime= as.POSIXct("2024-02-20 16:22:31 UTC", tz="UTC")
  )
  expect_equal(parseFilename(files, format="AudioMoth", timezone="UTC"), data)
  expect_equal(parseFilename(files, format="AudioMoth"), data)

  files <- "TEST_20240220_162231.wav"
  data <- list(
    filename="TEST_20240220_162231.wav",
    match="Wildlife Acoustics SM2",
    datetime= as.POSIXct("2024-02-20 16:22:31 UTC", tz="UTC"),
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
    datetime= as.POSIXct("2024-02-21 15:38:02 GMT")
  )
  expect_equal(parseFilename(files, timezone="GMT"), data)
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
      datetime=as.POSIXct("2022-04-30 UTC", tz="UTC")
    ),
    list(
      filename="20240121.wav",
      match="YYYYMMDD",
      datetime=as.POSIXct("2024-01-21 UTC", tz="UTC")
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
      datetime=as.POSIXct("1987-01-31 14:03:28 UTC", tz="UTC")
    ),
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime=as.POSIXct("2020-04-10 16:54:44 UTC", tz="UTC")
    )
  )
  expect_equal(parseFilename(files, format="match", timezone="UTC"), data)
})
