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

  expect_null(
    .detectFormat("__!__.mp3")
  )
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
      datetime= as.POSIXct("2020-04-11 01:54:44 JST", tz="Japan")
  )
  expect_equal(parseFilename(files, timezone="Japan"), data)

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
})
