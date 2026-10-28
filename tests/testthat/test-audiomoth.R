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
  d <- readMetadata(f)$sources$audiomoth
  expect_equal(typeof(d), "list")
  expect_true(all(c("raw", "start_time", "start_date", "time_zone", "serial") %in% names(d)))
  expect_equal(d$serial, "240435055C7B17DB")
  expect_equal(d$gain, "medium")

  f <- system.file("extdata/EMPTY.WAV", package="sonicscrewdriver")
  expect_null(readMetadata(f)$sources$audiomoth)
})

test_that("audiomothWave() is deprecated but still reads a file", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  expect_warning(audiomothWave(f), "deprecated")
  d <- suppressWarnings(audiomothWave(f))
  expect_equal(d, readMetadata(f)$sources$audiomoth)

  f <- system.file("extdata/EMPTY.WAV", package="sonicscrewdriver")
  expect_equal(suppressWarnings(audiomothWave(f)), list())
})

# A wave carrying an AudioMoth comment in its list. The chunk order is the
# argument of the test that uses it: the comment is found by reading the list
# wherever it sits, so a file whose list trails its audio reads the same as one
# whose list comes first. Reading the head of the file instead found only the
# latter, and silently reported no AudioMoth metadata for the former.
audiomothWav <- function(comment, listFirst=TRUE) {
  info <- mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("ICMT", comment)))
  chunks <- if (listFirst) list(info, mdData()) else list(mdData(), info)
  return(mdWav(mdFmt(), chunks[[1]], chunks[[2]]))
}

audiomothWavFile <- function(...) {
  f <- tempfile(fileext=".WAV")
  writeBin(audiomothWav(...), f)
  return(f)
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
    function(v) .audiomothComment(c(paste0(preambles[[v]], clauses[[v]])))
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
  d <- .audiomothComment(c(paste0(
    preambles[["1.6.0"]],
    " Frequency trigger (16.0kHz and window length of 16 samples) threshold",
    " was 10% with 1s minimum trigger duration.",
    " Low-pass filter with frequency of 5.0kHz applied."
  )))
  expect_equal(d$filter, "Low-pass")
  expect_equal(d$filter.limit, "5.0")
})

test_that("Deployment ID is read in place of the serial number", {
  d <- .audiomothComment(c(paste0(
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
    d <- .audiomothComment(c(paste0(
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
    d <- .audiomothComment(c(paste0(
      "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
      g, " gain while battery was 4.1V and temperature was 5.8C."
    )))
    expect_equal(d$gain, g)
  }
})

test_that("Gain index from firmware before 1.4.0 is read", {
  d <- .audiomothComment(c(paste0(
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
  d <- .audiomothComment(c(paste0(
    preambles[["1.6.0"]], " Recording stopped by magnetic switch."
  )))
  expect_equal(d$cancelled, "magnetic switch")

  d <- .audiomothComment(c(paste0(
    preambles[["1.6.0"]], " Recording stopped due to SD card write error."
  )))
  expect_equal(d$cancelled, "SD card write error")
})

test_that("Battery states outside the measured range are reported", {
  d <- .audiomothComment(c(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was less than 2.5V and temperature was 5.8C."
  )))
  expect_equal(d$voltage, "<2.5")

  d <- .audiomothComment(c(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was greater than 4.9V and temperature was 5.8C."
  )))
  expect_equal(d$voltage, ">4.9")
})

test_that("Battery thresholds written by firmware before 1.4.0 are reported", {
  # The comparison wording and the threshold both changed more than once, so
  # each era's pair is checked against the firmware that wrote it.
  legacy <- list(
    # 1.0 to 1.1.0
    list("battery state was < 3.6V", "<3.6"),
    list("battery state was > 5.0V", ">5.0"),
    # 1.2.0, which ended the comment with a full stop
    list("battery state was < 3.6V.", "<3.6"),
    list("battery state was > 5.0V.", ">5.0"),
    # 1.2.2 and 1.3.0
    list("battery state was less than 3.6V.", "<3.6"),
    list("battery state was greater than 4.9V.", ">4.9")
  )
  for (case in legacy) {
    d <- .audiomothComment(c(paste0(
      "Recorded at 23:32:00 01/05/2018 (UTC) by AudioMoth 240435055C7B17DB ",
      "at gain setting 2 while ", case[[1]]
    )))
    expect_equal(d$voltage, case[[2]])
  }
})

test_that("Measured voltage is read whatever its precision", {
  # Firmware 1.0 wrote two decimal places, every later version writes one.
  d <- .audiomothComment(c(paste0(
    "Recorded at 23:32:00 01/05/2018 by AudioMoth 240435055C7B17DB at gain ",
    "setting 2 while battery state was 4.02V"
  )))
  expect_equal(d$voltage, "4.02")

  d <- .audiomothComment(c(paste0(
    "Recorded at 23:32:00 01/05/2019 (UTC) by AudioMoth 240435055C7B17DB at ",
    "gain setting 2 while battery state was 4.1V."
  )))
  expect_equal(d$voltage, "4.1")
})

test_that("Negative temperatures are read", {
  d <- .audiomothComment(c(paste0(
    "Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ",
    "medium gain while battery was 4.1V and temperature was -5.8C."
  )))
  expect_equal(d$temperature, "-5.8")
})

test_that("Time zone offsets are read", {
  for (tz in c("UTC", "UTC+1", "UTC-5", "UTC+5:30")) {
    d <- .audiomothComment(c(paste0(
      "Recorded at 23:32:00 01/05/2021 (", tz, ") by AudioMoth ",
      "240435055C7B17DB at medium gain while battery was 4.1V and ",
      "temperature was 5.8C."
    )))
    expect_equal(d$time_zone, tz)
  }
})

test_that("The comment is read wherever the list sits in the file", {
  comment <- preambles[["1.6.0"]]
  for (listFirst in c(TRUE, FALSE)) {
    f <- audiomothWavFile(comment, listFirst=listFirst)
    m <- readMetadata(f)
    expect_equal(m$sources$audiomoth$serial, "240435055C7B17DB")
    expect_equal(m$sources$audiomoth$gain, "medium")
    expect_equal(m$fields$make, "Open Acoustic Devices")
  }
})

test_that("Reading a trailing list costs no warnings", {
  # Scanning the head of the file for the comment read the audio instead, and
  # warned once per string it broke off part way through.
  f <- audiomothWavFile(preambles[["1.6.0"]], listFirst=FALSE)
  expect_silent(readMetadata(f))
})

test_that("The comment is read from bytes as well as from a file", {
  bytes <- audiomothWav(preambles[["1.6.0"]])
  m <- readMetadata(bytes)
  expect_equal(m$sources$audiomoth$serial, "240435055C7B17DB")
})

test_that("An ID3 comment carries the AudioMoth record too", {
  # Some writers copy the recorder's comment into an ID3 tag and leave the
  # list behind; the record is the same wherever it is written down.
  comment <- c(
    as.raw(3), charToRaw("eng"), as.raw(0), mdUtf8(preambles[["1.6.0"]])
  )
  tag <- mdChunk("id3 ", mdId3Tag(mdFrame24("COMM", comment)))
  m <- readMetadata(mdWav(mdFmt(), mdData(), tag))
  expect_equal(m$sources$audiomoth$serial, "240435055C7B17DB")
})

test_that("A wave with no AudioMoth comment has no audiomoth source", {
  m <- readMetadata(mdWav(mdFmt(), mdData()))
  expect_null(m$sources$audiomoth)

  m <- readMetadata(mdWav(
    mdFmt(), mdData(),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("ICMT", "Just a comment")))
  ))
  expect_null(m$sources$audiomoth)
})

test_that("A file that is not there is still an error", {
  # Reading the head of the file failed loudly on a path that did not exist;
  # reading a chunk must not turn that into an empty result.
  expect_error(
    suppressWarnings(audiomothWave(file.path(tempdir(), "not-here.WAV"))),
    "does not exist"
  )
})

test_that("A file that is not a wave has no AudioMoth comment", {
  f <- tempfile(fileext=".WAV")
  writeLines("not audio at all", f)
  expect_equal(suppressWarnings(audiomothWave(f)), list())
})

test_that("An AudioMoth recording transcoded to MP3 is read", {
  # iNaturalist holds AudioMoth recordings transcoded to MP3, which keep the
  # recorder's comment in the tag. The comment is the comment whatever the
  # container, so it is read there too.
  comment <- c(
    as.raw(3), charToRaw("eng"), as.raw(0), mdUtf8(preambles[["1.6.0"]])
  )
  f <- mdFile(c(mdId3Tag(mdFrame24("COMM", comment)), mdMp3Frame()), ".mp3")
  m <- readMetadata(f, filename=FALSE)
  expect_equal(m$container, "MP3")
  expect_equal(m$sources$audiomoth$serial, "240435055C7B17DB")
  expect_equal(m$fields$make, "Open Acoustic Devices")
  expect_false(is.null(m$fields$datetime))
})

test_that("An ordinary MP3 has no audiomoth source", {
  f <- mdFile(c(mdId3Tag(mdTextFrame("TIT2", "A Title")), mdMp3Frame()), ".mp3")
  expect_null(readMetadata(f, filename=FALSE)$sources$audiomoth)
})

test_that("A version 1 tag gives what its 30-byte comment leaves", {
  # iNaturalist holds AudioMoth recordings transcoded to MP3 with a version 1
  # tag. The format allows the comment 30 bytes, which the recorder's wording
  # fills before it reaches the date, so the recorder is identified and the
  # time of day survives while the rest is genuinely not in the file.
  tag <- c(
    charToRaw("TAG"), raw(30), mdPad("AudioMoth 240435055C7B17DB", 30L),
    raw(30), raw(4), mdPad("Recorded at 03:00:00 03/06/202", 30L), as.raw(12)
  )
  f <- mdFile(c(mdMp3Frame(), tag), ".mp3")
  m <- readMetadata(f, filename=FALSE)

  expect_equal(m$sources$audiomoth$start_time, "03:00:00")
  expect_equal(m$fields$make, "Open Acoustic Devices")
  expect_equal(m$fields$model, "AudioMoth")
  # The cut falls inside the year, so there is no date and so no datetime, and
  # the serial is past the cut too. Reporting them as absent is the point.
  expect_true(is.na(m$sources$audiomoth$start_date))
  expect_true(is.na(m$sources$audiomoth$serial))
  expect_null(m$fields$datetime)
})
