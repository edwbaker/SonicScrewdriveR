test_that(".detectFormat() correctly identifies formats", {
  expect_equal(
    .detectFormat("5E90A4D4.wav"),
    "AudioMoth HEX"
  )
})

test_that("parseFilename() works as expected", {
  files <- c(
    "5E90A4D4.wav"
  )
  data <- c(
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime=as.POSIXct("2020-04-10 16:54:44 UTC", tz="UTC")
    )
  )
  expect_equal(parseFilename(files, timezone="UTC"), data)

  data <- c(
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime= as.POSIXct("2020-04-11 01:54:44 JST", tz="Japan")
    )
  )
  expect_equal(parseFilename(files, timezone="Japan"), data)
})
