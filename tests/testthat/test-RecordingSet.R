#A short wave file, written into dir under name
rsWav <- function(dir, name) {
  path <- file.path(dir, name)
  tuneR::writeWave(tuneR::sine(440, duration=100, samp.rate=8000), path)
  return(path)
}

#A small RecordingSet from a data frame, with whatever columns are given
rsFrame <- function(...) {
  recordingSet(data.frame(..., stringsAsFactors=FALSE))
}

# ------------------------------------------------------------- from files ---

test_that("a set made from files has every column, in order and of its type", {
  dir <- withr::local_tempdir()
  files <- c(rsWav(dir, "a.wav"), rsWav(dir, "b.wav"))
  rs <- recordingSet(files, source="survey")

  expect_s3_class(rs, c("RecordingSet", "data.frame"), exact=TRUE)
  expect_identical(names(rs), names(.recordingSetColumns))
  for (col in names(.recordingSetColumns)) {
    expect_identical(class(rs[[col]]), class(.recordingSetColumns[[col]]), info=col)
  }
  expect_identical(attr(rs$start, "tzone"), "UTC")
  expect_identical(rs$location, normalizePath(files, winslash="/"))
  expect_identical(rs$uri, .fileUri(rs$location))
  expect_true(all(startsWith(rs$uri, "file://")))
  expect_identical(rs$source, c("survey", "survey"))
  expect_true(all(is.na(rs$hash)))
  expect_true(all(is.na(rs$duration)))
  expect_identical(rownames(rs), c("1", "2"))
})

test_that("files can each be given a source, or none", {
  dir <- withr::local_tempdir()
  files <- c(rsWav(dir, "a.wav"), rsWav(dir, "b.wav"))

  expect_identical(recordingSet(files, source=c("x", "y"))$source, c("x", "y"))
  expect_identical(recordingSet(files)$source, c(NA_character_, NA_character_))
  expect_error(recordingSet(files, source=c("x", "y", "z")), "one value for each file")
  expect_error(recordingSet(files, source=character(0)), "one value for each file")
})

test_that("a file that is not there, a directory and NA are refused by name", {
  dir <- withr::local_tempdir()
  wav <- rsWav(dir, "a.wav")

  expect_error(recordingSet(file.path(dir, "nope.wav")), "1 file does not exist: .*nope\\.wav\\.$")
  expect_error(
    recordingSet(file.path(dir, paste0("nope", 1:4, ".wav"))),
    "4 files do not exist: .*nope1\\.wav, .*nope2\\.wav, .*nope3\\.wav, \\.\\.\\.\\.$"
  )
  expect_error(recordingSet(c(wav, dir)), "1 file does not exist")
  expect_error(recordingSet(c(wav, NA)), "cannot be NA")
})

test_that("one file given twice is one recording given twice, which is refused", {
  dir <- withr::local_tempdir()
  wav <- rsWav(dir, "a.wav")
  expect_error(recordingSet(c(wav, wav)), "1 uri appears more than once")
})

test_that("no files make an empty set", {
  rs <- recordingSet(character(0))
  expect_s3_class(rs, "RecordingSet")
  expect_equal(nrow(rs), 0)
  expect_identical(names(rs), names(.recordingSetColumns))
})

test_that("the address of a file names it and no other", {
  expect_identical(.fileUri("/home/a b/c#d.wav"), "file:///home/a%20b/c%23d.wav")
  expect_identical(.fileUri("C:/My Files/x.wav"), "file:///C:/My%20Files/x.wav")
  expect_identical(.fileUri("//server/share/x.wav"), "file://server/share/x.wav")
  #A percent sign that already looks like an escape is escaped again
  expect_identical(.fileUri("/a/100%25.wav"), "file:///a/100%2525.wav")
  #Only a drive at the start keeps its colon
  expect_identical(.fileUri("/a/x:/y.wav"), "file:///a/x%3A/y.wav")
  expect_identical(.fileUri(c("/a", "/b")), c("file:///a", "file:///b"))
  expect_identical(.fileUri(character(0)), character(0))
})

# -------------------------------------------------------- from a data frame ---

test_that("a data frame needs a uri and a location", {
  expect_error(recordingSet(data.frame(location="x")), "needs a uri column")
  expect_error(recordingSet(data.frame(uri="x")), "needs a location column")
  rs <- rsFrame(uri="u", location=NA)
  expect_s3_class(rs, "RecordingSet")
  expect_true(is.na(rs$location))
})

test_that("every recording needs a uri of its own", {
  expect_error(rsFrame(uri=c("u", NA), location="x"), "and 1 have none")
  expect_error(rsFrame(uri=c("u", " "), location="x"), "and 1 have none")
  expect_error(rsFrame(uri=c("u", "u"), location="x"), "1 uri appears more than once, e.g. u\\.")
  expect_error(
    rsFrame(uri=c("u", "u", "v", "v"), location="x"),
    "2 uris appear more than once, e.g. u\\."
  )
})

test_that("the columns of a set are converted to the types it holds", {
  rs <- rsFrame(
    uri="u", location="x", duration="12.5", sample_rate=44100L, channels="2",
    lat=factor("51.5"), hash="ABCDEF", author="", source=factor("xc")
  )
  expect_identical(rs$duration, 12.5)
  expect_identical(rs$sample_rate, 44100)
  expect_identical(rs$channels, 2L)
  expect_identical(rs$lat, 51.5)
  expect_identical(rs$hash, "abcdef")
  expect_identical(rs$author, NA_character_)
  expect_identical(rs$source, "xc")
  expect_identical(rsFrame(uri="u", location="x", duration="")$duration, NA_real_)
})

test_that("a value that is not a number is refused rather than lost", {
  expect_error(rsFrame(uri="u", location="x", duration="abc"),
               "duration column holds values that are not numbers, e.g. abc")
  expect_error(rsFrame(uri="u", location="x", channels=2.5),
               "channels column holds values that are not whole numbers, e.g. 2.5")
})

test_that("a start time is kept as the same instant in UTC, and must be one", {
  t <- as.POSIXct("2024-06-01 06:00:00", tz="Europe/London")
  rs <- recordingSet(data.frame(uri="u", location="x", start=t))
  expect_identical(attr(rs$start, "tzone"), "UTC")
  expect_equal(as.numeric(rs$start), as.numeric(t))

  expect_error(rsFrame(uri="u", location="x", start="2024-06-01 06:00"), "must hold dates and times")
  rs <- rsFrame(uri="u", location="x", start=NA)
  expect_true(is.na(rs$start))
  expect_s3_class(rs$start, "POSIXct")
})

test_that("other columns follow those of the set, in the order given", {
  rs <- rsFrame(zeta=1, location="x", alpha="a", uri="u")
  expect_identical(names(rs), c(names(.recordingSetColumns), "zeta", "alpha"))
})

test_that("a set made into a set again is unchanged", {
  rs <- rsFrame(uri=c("u", "v"), location="x", extra=1:2)
  expect_identical(recordingSet(rs), rs)
})

test_that("nothing else can be made into a set", {
  expect_error(recordingSet(1), "not from numeric")
  expect_error(recordingSet(NULL), "not from NULL")
})

# ------------------------------------------------------------- subsetting ---

test_that("a subset stays a set while it has a uri and a location", {
  rs <- rsFrame(uri=c("a", "b", "c"), location=c("x", "y", "z"), duration=1:3)

  expect_s3_class(rs[1:2, ], "RecordingSet")
  expect_s3_class(rs[rs$duration > 1, ], "RecordingSet")
  expect_s3_class(rs[, c("uri", "location", "duration")], "RecordingSet")

  plain <- rs[c("duration", "source")]
  expect_s3_class(plain, "data.frame", exact=TRUE)
  expect_s3_class(rs[, c("uri", "duration")], "data.frame", exact=TRUE)
  expect_identical(rs[, "duration"], c(1, 2, 3))
})

# -------------------------------------------------------------- combining ---

test_that("sets are combined on all of their columns", {
  t <- as.POSIXct("2024-06-01 06:00:00", tz="UTC")
  a <- rsFrame(uri=c("a1", "a2"), location=c("x", "y"), only_a=c("p", "q"))
  b <- recordingSet(data.frame(uri="b1", location="z", only_b=5, duration=7, start=t))
  ab <- rbind(a, b)

  expect_s3_class(ab, "RecordingSet")
  expect_identical(ab$uri, c("a1", "a2", "b1"))
  expect_identical(names(ab), c(names(.recordingSetColumns), "only_a", "only_b"))
  expect_identical(ab$only_a, c("p", "q", NA))
  expect_identical(ab$only_b, c(NA, NA, 5))
  expect_identical(ab$duration, c(NA, NA, 7))
  expect_equal(as.numeric(ab$start), c(NA, NA, as.numeric(t)))
  expect_identical(attr(ab$start, "tzone"), "UTC")
})

test_that("a set combines with a data frame, files and nothing", {
  dir <- withr::local_tempdir()
  wav <- rsWav(dir, "a.wav")
  a <- rsFrame(uri="a1", location="x")

  expect_identical(rbind(a, NULL), a)
  expect_identical(rbind(a, data.frame(uri="d1", location="w"))$uri, c("a1", "d1"))
  combined <- rbind(a, wav)
  expect_identical(combined$location, c("x", normalizePath(wav, winslash="/")))
})

test_that("a recording in two of the sets combined is refused", {
  a <- rsFrame(uri=c("a1", "a2"), location="x")
  expect_error(rbind(a, rsFrame(uri="a2", location="y")), "appears more than once, e.g. a2")
})

# ------------------------------------------------------------------ files ---

test_that("recordings sharing a hash or a location are of one file", {
  rs <- rsFrame(
    uri=paste0("u", 1:5),
    location=c("l1", "l1", "l2", "l3", NA),
    hash=c(NA, NA, "h1", "h1", NA)
  )
  expect_identical(.recordingSetFiles(rs), c(1L, 1L, 2L, 2L, 3L))
})

test_that("recordings are of one file through others", {
  #1 and 2 share a location, 2 and 3 a hash, and 3 and 4 a location
  rs <- rsFrame(
    uri=paste0("u", 1:5),
    location=c("A", "A", "B", "B", "C"),
    hash=c(NA, "H", "H", NA, NA)
  )
  expect_identical(.recordingSetFiles(rs), c(1L, 1L, 1L, 1L, 2L))
  expect_identical(.recordingSetFiles(recordingSet(character(0))), integer(0))
})

# --------------------------------------------------------------- printing ---

test_that("a set prints what it holds", {
  rs <- rsFrame(uri=c("a", "b", "c"), location=c("x", "x", "y"), source=c("s1", "s2", NA))
  expect_output(print(rs), "<RecordingSet: 3 recordings of 2 files, from 2 sources>")
  expect_output(print(rs), "Other columns: location, hash")
  expect_output(print(rs, n=2), "\\.\\.\\. and 1 more")
  utils::capture.output(printed <- withVisible(print(rs)))
  expect_false(printed$visible)
  expect_identical(printed$value, rs)

  one <- rsFrame(uri="a", location="x", source="s")
  expect_output(print(one), "<RecordingSet: 1 recording of 1 file, from 1 source>")
  expect_output(print(recordingSet(character(0))), "0 recordings of 0 files, from 0 sources")

  #A column taken out with $<- leaves the class, and the set still prints
  rs$status <- NULL
  expect_output(print(rs), "3 recordings of 2 files")
})

# -------------------------------------------------------------- audioBlast ---

#Answers every request with the records given, counting the requests made
rsAudioblast <- function(records, env=parent.frame()) {
  asked <- new.env(parent=emptyenv())
  asked$urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    asked$urls <- c(asked$urls, req$url)
    httr2::response_json(body=list(data=records, last_page=1, params=list()))
  }, env=env)
  return(asked)
}

test_that("audioBlast gives its recordings as a set", {
  shared <- "https://files.audioblast.org/tsa/1.mp3"
  api <- rsAudioblast(list(
    list(source="bio.acousti.ca", id="10001", filename="https://files.audioblast.org/b/a.wav",
         taxon="Chorthippus lacustris", author="Fer Willemse",
         license="https://creativecommons.org/licenses/by/4.0/", rights_holder="NHM",
         duration="96", sample_rate="44100", channels="2", date="1987-08-11",
         calculated_hash="ABCDEF", calculated_duration="95.5", calculated_sample_rate="22050",
         calculated_channels="1", calculated_codec="pcm_s16le", calculated_status="ok",
         calculated_by="audioBlastAnalyse 0.0.0.9002"),
    list(source="xeno-canto", id="767528", filename="https://xeno-canto.org/767528/download",
         author="", license="", duration="30", sample_rate="48000", channels="2",
         lat="51.5", lon="-0.1"),
    list(source="TSA", id="a/b", filename=shared, calculated_hash="h1", calculated_status="ok"),
    list(source="TSA", id="c d", filename=shared, calculated_hash="h1", calculated_status="ok")
  ))
  rs <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet")

  expect_length(api$urls, 1)
  expect_s3_class(rs, "RecordingSet")
  expect_identical(rs$uri, c(
    "https://api.audioblast.org/recording/bio.acousti.ca/10001",
    "https://api.audioblast.org/recording/xeno-canto/767528",
    "https://api.audioblast.org/recording/TSA/a/b",
    "https://api.audioblast.org/recording/TSA/c%20d"
  ))
  expect_identical(rs$source, c("bio.acousti.ca", "xeno-canto", "TSA", "TSA"))
  expect_identical(rs$location[1:3], c("https://files.audioblast.org/b/a.wav",
                                       "https://xeno-canto.org/767528/download", shared))
  expect_identical(rs$hash, c("abcdef", NA, "h1", "h1"))
  expect_identical(rs$status, c("ok", NA, "ok", "ok"))
  expect_identical(rs$codec, c("pcm_s16le", NA, NA, NA))

  #Measured where audioBlast has measured the file, declared otherwise
  expect_identical(rs$duration, c(95.5, 30, NA, NA))
  expect_identical(rs$sample_rate, c(22050, 48000, NA, NA))
  expect_identical(rs$channels, c(1L, 2L, NA, NA))
  expect_identical(rs$declared_duration, c("96", "30", NA, NA))
  expect_identical(rs$declared_sample_rate, c("44100", "48000", NA, NA))
  expect_identical(rs$declared_channels, c("2", "2", NA, NA))

  expect_identical(rs$lat, c(NA, 51.5, NA, NA))
  expect_identical(rs$lon, c(NA, -0.1, NA, NA))
  expect_identical(rs$author, c("Fer Willemse", NA, NA, NA))
  expect_identical(rs$license, c("https://creativecommons.org/licenses/by/4.0/", NA, NA, NA))
  expect_identical(rs$taxon, c("Chorthippus lacustris", NA, NA, NA))
  expect_identical(rs$rights_holder, c("NHM", NA, NA, NA))

  #Every other field is kept under its own name, and none of those used
  expect_true(all(c("id", "date", "calculated_by") %in% names(rs)))
  expect_false(any(c("filename", "calculated_hash", "calculated_duration") %in% names(rs)))

  expect_output(print(rs), "4 recordings of 3 files, from 3 sources")
})

test_that("a recording audioBlast gives no address for is left out, and said to be", {
  rsAudioblast(list(
    list(source="xeno-canto", id="1", filename="https://xeno-canto.org/1/download"),
    list(source="xeno-canto", filename="https://xeno-canto.org/2/download"),
    list(id="3", filename="https://xeno-canto.org/3/download")
  ))
  expect_message(
    rs <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet"),
    "gave 2 recordings without a source or an ID, which are left out"
  )
  expect_identical(rs$uri, "https://api.audioblast.org/recording/xeno-canto/1")

  rsAudioblast(list(list(source="xeno-canto", filename="https://xeno-canto.org/2/download")))
  expect_message(
    rs <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet"),
    "gave 1 recording without a source or an ID, which is left out"
  )
  expect_equal(nrow(rs), 0)
})

test_that("a recording audioBlast gives twice is kept once, as it came first", {
  rsAudioblast(list(
    list(source="iNaturalist", id="1", filename="first"),
    list(source="iNaturalist", id="2", filename="f2"),
    list(source="iNaturalist", id="1", filename="second"),
    list(source="iNaturalist", id="2", filename="f2"),
    list(source="iNaturalist", filename="no id"),
    list(source="iNaturalist", filename="no id either")
  ))
  expect_message(
    expect_message(
      rs <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet"),
      "gave 2 recordings more than once"
    ),
    "gave 2 recordings without a source or an ID"
  )
  expect_identical(rs$location, c("first", "f2"))

  rsAudioblast(list(list(source="s", id="1", filename="a"), list(source="s", id="1", filename="a")))
  expect_message(
    rs <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet"),
    "gave 1 recording more than once"
  )
  expect_equal(nrow(rs), 1)
})

test_that("measured numbers that came as numbers are used as they are", {
  #As they come once the module's information has been fetched
  d <- data.frame(source="s", id="1", filename="f", duration=96, calculated_duration=95.5,
                  sample_rate=44100, calculated_sample_rate=NA, stringsAsFactors=FALSE)
  rs <- .audioblast_recordingSet(d)
  expect_identical(rs$duration, 95.5)
  expect_identical(rs$sample_rate, 44100)
  expect_identical(rs$declared_duration, 96)
})

test_that("only a search of the recordings module gives a set, and nothing is asked", {
  api <- rsAudioblast(list())
  expect_error(
    audioblast("data", "annomate", check=FALSE, output="RecordingSet"),
    "Only a search of the recordings module gives a RecordingSet"
  )
  expect_error(
    audioblast("data", "recordings", "classification", check=FALSE, output="RecordingSet"),
    "Only a search of the recordings module gives a RecordingSet"
  )
  expect_error(
    audioblast("analysis", "recordings", check=FALSE, output="RecordingSet"),
    "Only a search of the recordings module gives a RecordingSet"
  )
  expect_length(api$urls, 0)
})

test_that("a search finding nothing gives NULL as a set", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body=list(data=list(), last_page=0, params=list()))
  })
  expect_null(audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="RecordingSet"))
})

test_that("a set from audioBlast is downloaded from its locations and checked against its hashes", {
  dir <- withr::local_tempdir()
  got <- character(0)
  local_mocked_bindings(.audioblast_download = function(url, path) {
    got <<- c(got, url)
    writeBin(charToRaw("not a wave"), path)
    invisible(path)
  })
  rs <- .audioblast_recordingSet(data.frame(
    source="xeno-canto", id=c("1", "2"), mime="audio/mpeg",
    filename=c("https://xeno-canto.org/1/download", "https://xeno-canto.org/2/download"),
    calculated_hash=c(NA, "0000"), stringsAsFactors=FALSE
  ))
  expect_warning(
    paths <- audioblastDownload(rs, dir=dir, metadata=TRUE, quiet=TRUE),
    "xeno-canto_2.mp3 does not match the hash"
  )
  expect_identical(got, rs$location)
  expect_identical(basename(paths), c("xeno-canto_1.mp3", "xeno-canto_2.mp3"))
  expect_true(file.exists(file.path(dir, "metadata.csv")))
})

test_that("a source or an ID is escaped for audioBlast, and a number written in full", {
  expect_identical(.audioblast_escape("a/b c"), "a/b%20c")
  expect_identical(.audioblast_escape(100000), "100000")
  expect_identical(.audioblast_escape(c(1, 25)), c("1", "25"))
  expect_identical(.audioblast_escape(c("plain-id_1.x~", paste0("caf", intToUtf8(233)), "a%41")),
                   c("plain-id_1.x~", "caf%C3%A9", "a%41"))
  expect_identical(.audioblast_escape(character(0)), character(0))

  asked <- new.env(parent=emptyenv())
  httr2::local_mocked_responses(function(req) {
    asked$url <- req$url
    httr2::response(status_code=404)
  })
  expect_null(audioblastRecord("recording", "bio.acousti.ca", 100000))
  expect_match(asked$url, "/recording/bio.acousti.ca/100000$")
})
