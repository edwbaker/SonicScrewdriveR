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
  expect_equal(d$serial, "240435055C7B17DB")
  expect_equal(d$gain, "medium")

  f <- system.file("extdata/EMPTY.WAV", package="sonicscrewdriver")
  expect_equal(audiomothWave(f), list())
})

# Writes a file containing an AudioMoth comment string, which is all
# audiomothWave() reads from the file.
audiomothFixture <- function(comment) {
  f <- tempfile(fileext=".WAV")
  con <- file(f, "wb")
  on.exit(close(con))
  writeBin(comment, con)
  f
}

# The part of the comment before any filter or cancellation clause, as each
# firmware era writes it. AudioMoth-Firmware-Basic 1.6.0 reworded "at medium
# gain setting while battery state was" to "at medium gain while battery was",
# so both wordings are in the field and both are tested throughout.
preambles <- list(
  "1.5.0" = paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain setting while battery state was 4.1V and temperature was 5.8C."
  ),
  "1.6.0" = paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was 4.1V and temperature was 5.8C."
  )
)

# A fixture for each firmware era, with clause appended to that era's preamble.
audiomothEras <- function(clauses=list("1.5.0"="", "1.6.0"="")) {
  lapply(
    stats::setNames(names(preambles), names(preambles)),
    function(v) audiomothWave(audiomothFixture(paste0(preambles[[v]], clauses[[v]])))
  )
}

test_that("Comment fields are read from both firmware wordings", {
  for (d in audiomothEras()) {
    expect_equal(d$start_time, "23:32:00")
    expect_equal(d$start_date, "01/05/2021")
    expect_equal(d$time_zone, "UTC")
    expect_equal(d$serial, "240435055C7B17DB")
    expect_true(is.na(d$deployment_id))
    expect_equal(d$gain, "medium")
    expect_equal(d$voltage, "4.1")
    expect_equal(d$temperature, "5.8")
    expect_false(d$external_mic)
    expect_false(d$filter)
    expect_false(d$cancelled)
  }
})

test_that("Serial number does not carry a trailing space", {
  for (d in audiomothEras()) {
    expect_equal(nchar(d$serial), 16)
  }
})

test_that("No filter applied", {
  for (d in audiomothEras(list("1.5.0"=" Amplitude threshold was 0.",
                               "1.6.0"=" Amplitude threshold was 0."))) {
    expect_false(d$filter)
    expect_false(d$filter.limit)
  }
})

test_that("Low-pass filter is parsed", {
  for (d in audiomothEras(list(
    "1.5.0" = " Low-pass filter applied with cut-off frequency of 5.0kHz.",
    "1.6.0" = " Low-pass filter with frequency of 5.0kHz applied."
  ))) {
    expect_equal(d$filter, "Low-pass")
    expect_equal(d$filter.limit, "5.0")
  }
})

test_that("High-pass filter is parsed", {
  for (d in audiomothEras(list(
    "1.5.0" = " High-pass filter applied with cut-off frequency of 1.5kHz.",
    "1.6.0" = " High-pass filter with frequency of 1.5kHz applied."
  ))) {
    expect_equal(d$filter, "High-pass")
    expect_equal(d$filter.limit, "1.5")
  }
})

test_that("Band-pass filter is parsed", {
  for (d in audiomothEras(list(
    "1.5.0" = paste0(
      " Band-pass filter applied with cut-off frequencies of 1.0kHz and 5.0kHz."
    ),
    "1.6.0" = " Band-pass filter with frequencies of 1.0kHz and 5.0kHz applied."
  ))) {
    expect_equal(d$filter, "Band-pass")
    expect_equal(d$filter.limit, "1.0-5.0")
  }
})

test_that("Filter frequency is not taken from the frequency trigger", {
  # Firmware from 1.7.0 writes the frequency trigger, which also carries a
  # value in kHz, before the filter clause.
  d <- audiomothWave(audiomothFixture(paste0(
    preambles[["1.6.0"]],
    " Frequency trigger (16.0kHz and window length of 16 samples) threshold",
    " was 10% with 1s minimum trigger duration.",
    " Low-pass filter with frequency of 5.0kHz applied."
  )))
  expect_equal(d$filter, "Low-pass")
  expect_equal(d$filter.limit, "5.0")
})

test_that("Deployment ID is read in place of the serial number", {
  d <- audiomothWave(audiomothFixture(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) during deployment 240435055C7B17DB ",
    "at medium gain while battery was 4.1V and temperature was 5.8C."
  )))
  expect_true(is.na(d$serial))
  expect_equal(d$deployment_id, "240435055C7B17DB")
  expect_equal(d$gain, "medium")
})

test_that("External microphone does not displace the gain", {
  for (clause in list(
    "medium gain setting while battery state was 4.1V.",
    "medium gain while battery was 4.1V and temperature was 5.8C."
  )) {
    d <- audiomothWave(audiomothFixture(paste0(
      "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB ",
      "using external microphone at ", clause
    )))
    expect_true(d$external_mic)
    expect_equal(d$gain, "medium")
    expect_equal(d$serial, "240435055C7B17DB")
  }
})

test_that("All gain settings are read", {
  for (g in c("low", "low-medium", "medium", "medium-high", "high")) {
    d <- audiomothWave(audiomothFixture(paste0(
      "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
      g, " gain while battery was 4.1V and temperature was 5.8C."
    )))
    expect_equal(d$gain, g)
  }
})

test_that("Gain index from firmware before 1.4.0 is read", {
  d <- audiomothWave(audiomothFixture(paste0(
    "Recorded at 23:32:00 01/05/2018 by AudioMoth 240435055C7B17DB at gain ",
    "setting 2 while battery state was 4.1V."
  )))
  expect_equal(d$gain, "2")
  expect_equal(d$serial, "240435055C7B17DB")
  expect_true(is.na(d$temperature))
})

test_that("Cancellation reasons are parsed from both wordings", {
  for (d in audiomothEras(list(
    "1.5.0" = " Recording cancelled before completion due to change of switch position.",
    "1.6.0" = " Recording stopped due to switch position change."
  ))) {
    expect_equal(d$cancelled, "change of switch position")
  }

  for (d in audiomothEras(list(
    "1.5.0" = " Recording cancelled before completion due to microphone change.",
    "1.6.0" = " Recording stopped due to microphone change."
  ))) {
    expect_equal(d$cancelled, "microphone change")
  }

  for (d in audiomothEras(list(
    "1.5.0" = " Recording cancelled before completion due to low voltage.",
    "1.6.0" = " Recording stopped due to low voltage."
  ))) {
    expect_equal(d$cancelled, "low voltage")
  }

  for (d in audiomothEras(list(
    "1.5.0" = " Recording cancelled before completion due to file size limit.",
    "1.6.0" = " Recording stopped due to file size limit."
  ))) {
    expect_equal(d$cancelled, "file size limit")
  }
})

test_that("Cancellation reasons added after 1.6.0 are parsed", {
  d <- audiomothWave(audiomothFixture(paste0(
    preambles[["1.6.0"]], " Recording stopped by magnetic switch."
  )))
  expect_equal(d$cancelled, "magnetic switch")

  d <- audiomothWave(audiomothFixture(paste0(
    preambles[["1.6.0"]], " Recording stopped due to SD card write error."
  )))
  expect_equal(d$cancelled, "SD card write error")
})

test_that("Battery states outside the measured range are reported", {
  d <- audiomothWave(audiomothFixture(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was less than 2.5V and temperature was 5.8C."
  )))
  expect_equal(d$voltage, "<2.5")

  d <- audiomothWave(audiomothFixture(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was greater than 4.9V and temperature was 5.8C."
  )))
  expect_equal(d$voltage, ">4.9")
})

test_that("Negative temperatures are read", {
  d <- audiomothWave(audiomothFixture(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was 4.1V and temperature was -5.8C."
  )))
  expect_equal(d$temperature, "-5.8")
})

test_that("Time zone offsets are read", {
  for (tz in c("UTC", "UTC+1", "UTC-5", "UTC+5:30")) {
    d <- audiomothWave(audiomothFixture(paste0(
      "Recorded at 23:32:00 01/05/2021 (", tz, ") by AudioMoth ",
      "240435055C7B17DB at medium gain while battery was 4.1V and ",
      "temperature was 5.8C."
    )))
    expect_equal(d$time_zone, tz)
  }
})
