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
      datetime=as.POSIXct("2020-04-10 17:54:44 BST")
    )
  )
  expect_equal(parseFilename(files), data)

  dt <- as.POSIXct("2020-04-11 01:54:44 JST", tz="Japan")
  data <- c(
    list(
      filename="5E90A4D4.wav",
      match="AudioMoth HEX",
      datetime=dt
    )
  )
  expect_equal(parseFilename(files, timezone="Japan"), data)
})
