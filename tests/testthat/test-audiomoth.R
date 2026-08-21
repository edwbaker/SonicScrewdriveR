test_that("Reading config file", {
  f <- system.file("extdata/CONFIG.TXT", package="sonicscrewdriver")
  t <- audiomothConfig(f)
  expect_equal(typeof(t), "list")
  expect_equal(ncol(t), 2)
  expect_equal(names(t), c("Key", "Value"))

  # Check for standard keys
  expect_true(all(c("Device ID", "Firmware", "Gain") %in% t$Key))
})

test_that("Reading data from audiomoth wav file", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  d <- audiomothWave(f)
  expect_silent(audiomothWave(f))
  expect_equal(typeof(d), "list")
  expect_true(all(c("raw", "start_time", "start_date", "time_zone", "serial") %in% names(d)))

  f <- system.file("extdata/EMPTY.WAV", package="sonicscrewdriver")
  expect_equal(audiomothWave(f), list())
})

# Writes a file containing an AudioMoth comment string, which is all
# audiomothWave() reads from the file.
audiomothFixture <- function(comment) {
  f <- tempfile(fileext=".WAV")
  con <- file(f, "wb")
  on.exit(close(con))
  writeBin(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain setting while battery state was 4.1V and temperature was 5.8C. ",
    comment
  ), con)
  f
}

test_that("No filter applied", {
  d <- audiomothWave(audiomothFixture("Amplitude threshold was 0."))
  expect_false(d$filter)
  expect_false(d$filter.limit)
})

test_that("Low-pass filter is parsed", {
  d <- audiomothWave(audiomothFixture("Low-pass filter applied with frequency of 5.0kHz."))
  expect_equal(d$filter, "Low-pass")
  expect_equal(d$filter.limit, "5.0")
})

test_that("High-pass filter is parsed", {
  d <- audiomothWave(audiomothFixture("High-pass filter applied with frequency of 1.5kHz."))
  expect_equal(d$filter, "High-pass")
  expect_equal(d$filter.limit, "1.5")
})

test_that("Band-pass filter is parsed", {
  d <- audiomothWave(audiomothFixture(
    "Band-pass filter applied with frequencies of 1.0kHz and 5.0kHz."
  ))
  expect_equal(d$filter, "Band-pass")
  expect_equal(d$filter.limit, "1.0-5.0")
})
