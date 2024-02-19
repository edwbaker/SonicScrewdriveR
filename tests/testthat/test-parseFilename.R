test_that(".detectFormat() correctly identifies formats", {
  expect_equal(
    .detectFormat("5E90A4D4.wav"),
    "AudioMoth HEX"
  )
  expect_null(
    .detectFormat("__!__.mp3")
  )
})

test_that("parseFilename() works as expected", {
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
})
