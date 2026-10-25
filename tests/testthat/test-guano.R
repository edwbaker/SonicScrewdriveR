# AUDIOMOTH_GUANO.WAV carries metadata of the shape current AudioMoth firmware
# writes, including the fields it puts in its own OAD namespace. The older
# AUDIOMOTH.WAV predates the firmware that writes GUANO and carries none.
test_that("The bundled metadata is read field by field", {
  f <- system.file("extdata/AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
  g <- guano(f)

  expect_s3_class(g, "guano")
  expect_length(g, 12)
  expect_identical(g[["GUANO|Version"]], "1.0")
  expect_identical(g[["Make"]], "Open Acoustic Devices")
  expect_identical(g[["Model"]], "AudioMoth")
  expect_identical(g[["Serial"]], "247AB4055C7B17DB")
  expect_identical(g[["Firmware Version"]], "AudioMoth-Firmware-Basic (1.11.0)")
  expect_equal(g[["Loc Position"]], c(51.4964, -0.1224))
  expect_identical(g[["Temperature Int"]], 12.3)

  # The timestamp was written with a UTC offset of one hour.
  expect_s3_class(g[["Timestamp"]], "POSIXct")
  expect_identical(format(g[["Timestamp"]], "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2026-05-14T20:06:00Z")

  # Fields in a manufacturer's namespace are kept exactly as written.
  expect_identical(g[["OAD|Battery Voltage"]], "4.1")
  expect_identical(g[["OAD|Loc Source"]], "Configuration app")
  expect_identical(g[["OAD|Recording Settings"]], "8000 GAIN 2 AMP 1000 1")
})

test_that("A recording from before GUANO was written carries none", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  expect_length(guano(f), 0)
})

test_that("Fields are read as the types the specification gives them", {
  g <- guanoParse(paste(
    "GUANO|Version:1.0",
    "Make:Open Acoustic Devices",
    "Samplerate:48000",
    "Length:10.5",
    "Loc Position:51.4964 -0.1224",
    "TE:10",
    "Tags:bat, roost, dusk",
    "Species Manual ID:Pipistrellus pipistrellus,Myotis daubentonii",
    "Timestamp:2020-11-03T11:24:30Z",
    sep = "\n"
  ))

  expect_s3_class(g, "guano")
  expect_identical(g[["Make"]], "Open Acoustic Devices")
  expect_identical(g[["Samplerate"]], 48000L)
  expect_identical(g[["Length"]], 10.5)
  expect_equal(g[["Loc Position"]], c(51.4964, -0.1224))
  expect_identical(g[["TE"]], 10L)
  expect_identical(g[["Tags"]], c("bat", "roost", "dusk"))
  expect_identical(
    g[["Species Manual ID"]],
    c("Pipistrellus pipistrellus", "Myotis daubentonii")
  )
  expect_s3_class(g[["Timestamp"]], "POSIXct")
  expect_identical(format(g[["Timestamp"]], "%Y-%m-%d %H:%M:%S", tz="UTC"), "2020-11-03 11:24:30")
})

test_that("The timestamp forms the specification allows are read", {
  stamp <- function(value) guanoParse(paste0("GUANO|Version:1.0\nTimestamp:", value))[["Timestamp"]]

  expect_identical(format(stamp("2020-11-03T11:24:30Z"), "%H:%M:%S", tz="UTC"), "11:24:30")
  expect_identical(format(stamp("2020-11-03T11:24:30+01:00"), "%H:%M:%S", tz="UTC"), "10:24:30")
  expect_identical(format(stamp("2020-11-03T11:24:30"), "%H:%M:%S"), "11:24:30")
  expect_identical(format(stamp("2020-11-03"), "%Y-%m-%d"), "2020-11-03")

  # A time with no date has nothing to attach it to, so it stays as text.
  expect_identical(stamp("11:24:30"), "11:24:30")
})

test_that("A timestamp is written back in a form that reads the same", {
  g <- guanoCreate(Timestamp=as.POSIXct("2020-11-03 11:24:30", tz="UTC"))
  expect_true(grepl("Timestamp:2020-11-03T11:24:30Z", as.character(g), fixed=TRUE))
  expect_equal(guanoParse(as.character(g))[["Timestamp"]], g[["Timestamp"]])
})

test_that("Unknown and namespaced fields are kept as written", {
  g <- guanoParse("GUANO|Version:1.0\nOAD|Battery Voltage:4.1\nXX|Thing:whatever")
  expect_identical(g[["OAD|Battery Voltage"]], "4.1")
  expect_identical(g[["XX|Thing"]], "whatever")
  expect_true(grepl("OAD|Battery Voltage:4.1", as.character(g), fixed=TRUE))
})

test_that("A field that cannot be read as its type is kept and written as text", {
  expect_warning(g <- guanoParse("GUANO|Version:1.0\nSamplerate:lots"), "as a number")
  expect_identical(g[["Samplerate"]], "lots")
  expect_true(grepl("Samplerate:lots", as.character(g), fixed=TRUE))

  expect_warning(g <- guanoParse("GUANO|Version:1.0\nTimestamp:whenever"), "timestamp")
  expect_identical(g[["Timestamp"]], "whenever")
  expect_true(grepl("Timestamp:whenever", as.character(g), fixed=TRUE))
})

test_that("Values keep colons and whitespace is trimmed", {
  g <- guanoParse("GUANO|Version:1.0\n  Note : a:b:c  ")
  expect_identical(g[["Note"]], "a:b:c")
})

test_that("Escaped newlines in a note are read and written back", {
  g <- guanoParse("GUANO|Version:1.0\nNote:first\\nsecond")
  expect_identical(g[["Note"]], "first\nsecond")
  expect_true(grepl("Note:first\\nsecond", as.character(g), fixed=TRUE))
})

test_that("Empty lines and lines with no value are skipped", {
  expect_warning(
    g <- guanoParse("GUANO|Version:1.0\n\nnonsense\nMake:X"),
    "no value"
  )
  expect_identical(names(g), c("GUANO|Version", "Make"))
})

test_that("The version field is written first", {
  g <- guanoCreate(Make="X")
  expect_identical(names(g)[1], "GUANO|Version")
  expect_true(startsWith(as.character(g), "GUANO|Version:1.0"))

  g <- guanoParse("Make:X\nGUANO|Version:1.0")
  expect_true(startsWith(as.character(g), "GUANO|Version:1.0"))
})

test_that("Metadata survives a write and read of a wave file", {
  w <- tuneR::Wave(left=as.integer(stats::rnorm(1000, sd=1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  expect_length(guano(f), 0)

  g <- guanoCreate(
    Make = "Open Acoustic Devices",
    Model = "AudioMoth",
    Samplerate = 8000,
    "Loc Position" = c(51.4964, -0.1224),
    Timestamp = as.POSIXct("2020-11-03 11:24:30", tz="UTC")
  )
  guanoWrite(g, f)

  back <- guano(f)
  expect_identical(back[["Make"]], "Open Acoustic Devices")
  expect_identical(back[["Samplerate"]], 8000L)
  expect_equal(back[["Loc Position"]], c(51.4964, -0.1224))
  expect_identical(format(back[["Timestamp"]], "%H:%M:%S", tz="UTC"), "11:24:30")

  # The audio must be untouched.
  expect_equal(tuneR::readWave(f)@left, w@left)
})

test_that("Writing metadata twice replaces it rather than adding a chunk", {
  w <- tuneR::Wave(left=as.integer(stats::rnorm(1000, sd=1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  guanoWrite(guanoCreate(Make="First", Note="a longer note than the next one"), f)
  size <- file.size(f)
  guanoWrite(guanoCreate(Make="Second"), f)

  expect_identical(guano(f)[["Make"]], "Second")
  expect_null(guano(f)[["Note"]])
  expect_lt(file.size(f), size)
  expect_equal(tuneR::readWave(f)@left, w@left)

  bytes <- readBin(f, "raw", n=file.size(f))
  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_equal(sum(ids == "guan"), 1)
})

test_that("Metadata can be written to a separate file", {
  w <- tuneR::Wave(left=as.integer(stats::rnorm(1000, sd=1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))
  tuneR::writeWave(w, f)

  guanoWrite(guanoCreate(Make="X"), f, out)
  expect_length(guano(f), 0)
  expect_identical(guano(out)[["Make"]], "X")
})

test_that("Metadata can be attached to a wave object", {
  w <- tuneR::Wave(left=integer(100), samp.rate=8000, bit=16)
  expect_length(guano(w), 0)

  guano(w) <- guanoCreate(Make="X")
  expect_identical(guano(w)[["Make"]], "X")

  guano(w) <- NULL
  expect_length(guano(w), 0)

  expect_error(`guano<-`("not a wave", guanoCreate(Make="X")), "Wave")
  expect_error(`guano<-`(w, list(Make="X")), "guano object")
})

test_that("Metadata survives expansion of a triggered file", {
  samples <- as.integer(round(stats::rnorm(8000 * 4, sd=20)))
  samples[8000:10000] <- 12000L
  w <- tuneR::Wave(left=samples, samp.rate=8000, bit=16, pcm=TRUE)

  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))

  g <- guanoCreate(Make="Open Acoustic Devices", Samplerate=8000)
  audiomothCompress(w, f, threshold=1000, segment=1024, guano=g)

  expect_identical(guano(f)[["Make"]], "Open Acoustic Devices")
  expect_identical(guano(audiomothExpand(f))[["Make"]], "Open Acoustic Devices")

  audiomothExpand(f, out)
  expect_identical(guano(out)[["Make"]], "Open Acoustic Devices")
  expect_equal(length(tuneR::readWave(out)@left), length(w@left))
})

test_that("Bad arguments are rejected", {
  expect_error(guano(1), "file path")
  expect_error(guano(tempfile()), "does not exist")
  expect_error(guanoParse(c("a", "b")), "single string")
  expect_error(guanoCreate("unnamed"), "named")
  expect_error(guanoWrite(list(), tempfile()), "guano object")
})

test_that("Metadata prints", {
  expect_output(print(guanoCreate(Make="X")), "GUANO|Version", fixed=TRUE)
  expect_output(print(guanoParse("")), "no GUANO metadata")
})

test_that("A timestamp away from UTC is written with its offset", {
  stamp <- as.POSIXct("2020-11-03 11:24:30", tz="Europe/Paris")
  g <- guanoCreate(Timestamp=stamp)
  written <- as.character(g)

  expect_match(written, "Timestamp:2020-11-03T11:24:30+01:00", fixed=TRUE)
  # And it reads back as the same moment.
  expect_equal(as.numeric(guanoParse(written)[["Timestamp"]]), as.numeric(stamp))
})

test_that("Metadata subsets as metadata and formats as its text", {
  g <- guanoCreate(Make="X", Model="Y", Samplerate=8000)

  part <- g[1:2]
  expect_s3_class(part, "guano")
  expect_identical(names(part), c("GUANO|Version", "Make"))

  expect_identical(format(g), as.character(g))
})

test_that("A field given twice is read as the last of them", {
  expect_warning(
    g <- guanoParse("GUANO|Version:1.0\nMake:First\nMake:Second"),
    "more than once"
  )
  expect_identical(g[["Make"]], "Second")
  expect_length(g, 2)
})

test_that("A line with an empty key is skipped", {
  g <- guanoParse("GUANO|Version:1.0\n:orphaned\nMake:X")
  expect_identical(names(g), c("GUANO|Version", "Make"))
})

test_that("Metadata reads from raw bytes, and passes through unchanged", {
  f <- system.file("extdata/AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
  bytes <- readBin(f, "raw", n=file.size(f))

  fromRaw <- guano(bytes)
  expect_identical(unclass(fromRaw), unclass(guano(f)))
  # Given metadata, guano() hands it straight back.
  expect_identical(guano(fromRaw), fromRaw)
})

test_that("Anything that is not a wave file carries no metadata", {
  expect_length(guano(charToRaw("not a wave file at all, by any measure")), 0)
})

test_that("An empty block of text is metadata with nothing in it", {
  g <- guanoParse("")
  expect_length(g, 0)
  # Writing it still produces the version field the specification requires.
  expect_identical(as.character(g), "GUANO|Version:1.0")
})

test_that("Metadata is replaced in place when another chunk follows it", {
  w <- tuneR::Wave(left=as.integer(stats::rnorm(1000, sd=1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  # Put metadata in, then a chunk after it, so that replacing the metadata has
  # to carry what follows across rather than simply truncating the file.
  guanoWrite(guanoCreate(Make="First", Note="a longer note than the next one"), f)
  bytes <- readBin(f, "raw", n=file.size(f))
  trailing <- c(charToRaw("junk"), .uintToRaw(8, 4L), charToRaw("12345678"))
  con <- file(f, "wb")
  writeBin(c(bytes, trailing), con)
  close(con)
  # The RIFF size has to count the chunk just added.
  bytes <- readBin(f, "raw", n=file.size(f))
  bytes[5:8] <- .uintToRaw(length(bytes) - 8, 4L)
  con <- file(f, "wb")
  writeBin(bytes, con)
  close(con)

  guanoWrite(guanoCreate(Make="Second"), f)

  expect_identical(guano(f)[["Make"]], "Second")
  ids <- vapply(.wavChunks(readBin(f, "raw", n=file.size(f)))$chunks,
                function(c) c$id, character(1))
  expect_equal(sum(ids == "guan"), 1)
  expect_true("junk" %in% ids)
  expect_equal(tuneR::readWave(f)@left, w@left)
})

test_that("Writing metadata to a file that is not there is reported", {
  expect_error(guanoWrite(guanoCreate(Make="X"), tempfile()), "does not exist")
})

test_that("A name used twice cannot be written", {
  g <- guanoCreate(Make="X")
  names(g) <- c("Make", "Make")
  expect_error(as.character(g), "more than once")

  g <- guanoCreate(Make="X")
  names(g)[2] <- "Bad\nName"
  expect_error(as.character(g), "newline")
})

test_that("A fractional second is kept, and a whole one is not invented", {
  stamp <- function(value) {
    text <- as.character(guanoParse(paste0("GUANO|Version:1.0\nTimestamp:", value)))
    sub(".*Timestamp:", "", strsplit(text, "\n")[[1]][2])
  }
  expect_identical(stamp("2020-11-03T11:24:30.123456Z"), "2020-11-03T11:24:30.123456Z")
  expect_identical(stamp("2020-11-03T11:24:30.5Z"), "2020-11-03T11:24:30.5Z")
  expect_identical(stamp("2020-11-03T11:24:30Z"), "2020-11-03T11:24:30Z")
})

test_that("Numbers are written to the precision they were read at", {
  g <- guanoParse("GUANO|Version:1.0\nLoc Position:51.496401 -0.122456\nLength:12.3456789")

  # What is written must not depend on how the session displays numbers.
  original <- options(digits=7)
  on.exit(options(original))
  for (digits in c(3, 7, 15)) {
    options(digits=digits)
    back <- guanoParse(as.character(g))
    expect_equal(back[["Loc Position"]], c(51.496401, -0.122456))
    expect_equal(back[["Length"]], 12.3456789)
  }
})

test_that("A fractional second keeps the zeros inside it", {
  stamp <- function(value) {
    text <- as.character(guanoParse(paste0("GUANO|Version:1.0\nTimestamp:", value)))
    sub(".*Timestamp:", "", strsplit(text, "\n")[[1]][2])
  }
  # Trimming the trailing zeros must not take the zeros within the fraction,
  # which would change the time rather than tidy it.
  expect_identical(stamp("2020-11-03T11:24:30.101234Z"), "2020-11-03T11:24:30.101234Z")
  expect_identical(stamp("2020-11-03T11:24:30.1002Z"), "2020-11-03T11:24:30.1002Z")
  expect_identical(stamp("2020-11-03T11:24:30.010001Z"), "2020-11-03T11:24:30.010001Z")
  expect_identical(stamp("2020-11-03T11:24:30.100000Z"), "2020-11-03T11:24:30.1Z")
})

test_that("Numbers are written with a dot whatever the session uses", {
  g <- guanoParse("GUANO|Version:1.0\nLength:12.5\nLoc Position:51.496401 -0.122456")

  original <- options(OutDec=",")
  on.exit(options(original))
  text <- as.character(g)
  options(original)

  expect_false(grepl("12,5", text, fixed=TRUE))
  back <- guanoParse(text)
  expect_identical(back[["Length"]], 12.5)
  expect_equal(back[["Loc Position"]], c(51.496401, -0.122456))
})

test_that("A timestamp given as a POSIXlt is written rather than raising", {
  # A POSIXlt carries its zone as three strings, the zone and the names of its
  # standard and summer time, which cannot be compared as one value.
  local <- as.POSIXlt(as.POSIXct("2020-07-03 11:24:30", tz="Europe/Paris"))
  expect_length(attr(local, "tzone"), 3)

  g <- guanoCreate(Timestamp=local)
  expect_match(as.character(g), "Timestamp:2020-07-03T11:24:30+02:00", fixed=TRUE)
  expect_output(print(g), "Timestamp", fixed=TRUE)
})

test_that("A timestamp naming no zone is not given one", {
  stamp <- function(value) {
    text <- as.character(guanoParse(paste0("GUANO|Version:1.0\nTimestamp:", value)))
    sub(".*Timestamp:", "", strsplit(text, "\n")[[1]][2])
  }
  # A field that named no zone must come back as it went in, whatever zone the
  # machine reading it is in.
  expect_identical(stamp("2020-11-03T11:24:30"), "2020-11-03T11:24:30")
  expect_identical(stamp("2020-11-03"), "2020-11-03")
  expect_identical(stamp("2020-11-03T11:24:30.1002"), "2020-11-03T11:24:30.1002")

  # One that did name a zone still does.
  expect_identical(stamp("2020-11-03T11:24:30Z"), "2020-11-03T11:24:30Z")
})

test_that("A timestamp naming no zone does not depend on the machine", {
  original <- Sys.getenv("TZ", unset=NA)
  on.exit(if (is.na(original)) Sys.unsetenv("TZ") else Sys.setenv(TZ=original))

  read <- function(zone) {
    Sys.setenv(TZ=zone)
    guanoParse("GUANO|Version:1.0\nTimestamp:2020-11-03T11:24:30")[["Timestamp"]]
  }
  expect_equal(as.numeric(read("UTC")), as.numeric(read("Australia/Sydney")))
  expect_equal(as.numeric(read("UTC")), as.numeric(read("America/Los_Angeles")))
})

test_that("Metadata written to a recording cut short can be read back", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  # Cut the audio short without correcting the size the file declares, as a
  # flat battery or a full card would.
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) - 1744)], con)
  close(con)

  expect_warning(guanoWrite(guanoCreate(Make="X"), f), "truncated")

  # Without the declared size being corrected the metadata would sit inside
  # the audio the file claims to hold, where nothing would find it.
  expect_identical(guano(f)[["Make"]], "X")
  info <- .wavChunks(readBin(f, "raw", n=file.size(f)))
  expect_false(is.null(.wavFindChunk(info$chunks, "guan")))
  expect_equal(info$riffSize + 8, file.size(f))
  expect_gt(length(tuneR::readWave(f)@left), 0)
})

test_that("Metadata is readable after a recording ending on an odd byte", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  # Leave an odd number of audio bytes, so that a padding byte is needed
  # before anything written after the audio.
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) - 1743)], con)
  close(con)
  info <- .wavChunks(readBin(f, "raw", n=file.size(f)))
  expect_equal(.wavFindChunk(info$chunks, "data")$size %% 2, 1)

  expect_warning(guanoWrite(guanoCreate(Make="X"), f), "truncated")

  # Without the padding byte the chunk would start one byte early and be read
  # under some other name.
  expect_identical(guano(f)[["Make"]], "X")
  ids <- vapply(.wavChunks(readBin(f, "raw", n=file.size(f)))$chunks,
                function(chunk) chunk$id, character(1))
  expect_true("guan" %in% ids)
  expect_equal(.wavChunks(readBin(f, "raw", n=file.size(f)))$riffSize + 8, file.size(f))
})

test_that("Reading metadata from a path gives what reading the bytes gives", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  # The file has no metadata yet, and must not be reported as an error.
  expect_identical(guano(f), guano(readBin(f, "raw", n=file.size(f))))

  g <- guanoCreate(
    Make = "Open Acoustic Devices",
    Samplerate = 8000,
    "Loc Position" = c(51.4964, -0.1224),
    Timestamp = as.POSIXct("2020-11-03 11:24:30", tz="UTC")
  )
  guanoWrite(g, f)
  expect_equal(guano(f), guano(readBin(f, "raw", n=file.size(f))))

  # A file that is not a wave file at all still gives empty metadata rather
  # than an error.
  writeLines("not a wave file at all", f)
  expect_length(guano(f), 0)
})

test_that("Replacing metadata held before the audio keeps the omitted periods", {
  # The specification allows the metadata chunk anywhere in the file. Where it
  # comes before the audio, replacing it with a chunk of a different size moves
  # the audio, and an AudioMoth triggered recording marks its omitted periods
  # by their position in the file, so moving it by anything but whole 512-byte
  # blocks loses them.
  source <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  bytes <- readBin(source, "raw", n=file.size(source))
  data <- .wavFindChunk(.wavChunks(bytes)$chunks, "data")

  body <- charToRaw(as.character(guanoCreate(Make="X")))
  body <- c(body, charToRaw(strrep(" ", 512 - 8 - length(body))))
  chunk <- c(charToRaw("guan"), .uintToRaw(length(body), 4L), body)
  at <- data$start - 9L
  bytes <- c(bytes[1:at], chunk, bytes[(at + 1L):length(bytes)])
  bytes[5:8] <- .uintToRaw(length(bytes) - 8, 4L)

  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  con <- file(f, "wb")
  writeBin(bytes, con)
  close(con)

  ids <- vapply(.wavChunks(bytes)$chunks, function(chunk) chunk$id, character(1))
  expect_equal(ids, c("fmt ", "LIST", "guan", "data"))
  expect_equal(length(audiomothExpand(f)@left), 49908)

  # Longer than what it replaces, and shorter, both leave the audio where it is.
  for (value in c("A much longer value than the one it replaces, by far", "s")) {
    guanoWrite(guanoCreate(Make=value), f)
    expect_identical(guano(f)[["Make"]], value)
    expect_equal(length(audiomothExpand(f)@left), 49908)
  }
})

test_that("A file that cannot be read is told apart from one holding nothing", {
  # A directory exists, so it passes the check that the file is there, but it
  # cannot be opened. Reporting that as metadata being absent would hide it.
  unreadable <- tempfile()
  dir.create(unreadable)
  on.exit(unlink(unreadable, recursive=TRUE))
  expect_error(suppressWarnings(guano(unreadable)))

  # A file that simply is not a wave file does hold no metadata, and says so.
  notAWave <- tempfile(fileext=".WAV")
  on.exit(unlink(notAWave), add=TRUE)
  writeLines("not a wave file at all, by any measure", notAWave)
  expect_length(guano(notAWave), 0)
})

test_that("A timestamp is read only where the whole field is one", {
  stamp <- function(value) {
    suppressWarnings(guanoParse(paste0("GUANO|Version:1.0\nTimestamp:", value))[["Timestamp"]])
  }
  # as.POSIXct() is content with a value that merely begins with a form it
  # knows, which would read a time of 25:00:00 as the date and drop the hour.
  expect_identical(stamp("2020-11-03T25:00:00Z"), "2020-11-03T25:00:00Z")
  expect_identical(stamp("2020-11-03rubbish"), "2020-11-03rubbish")

  expect_s3_class(stamp("2020-11-03T11:24:30Z"), "POSIXct")
  expect_s3_class(stamp("2020-11-03"), "POSIXct")
  expect_s3_class(stamp("2020-11-03T11:24:30+01:00"), "POSIXct")
})

test_that("A value cannot carry a field of its own into the file", {
  # A field is a line and a name ends at its first colon, so a newline in a
  # value would be read back as fields of its own and could rewrite others.
  expect_error(
    as.character(guanoCreate(Make="Original", "User|Comment"="first\nMake:Replaced")),
    "must not contain a newline"
  )
  expect_error(as.character(guanoCreate("Bad:Key"="x")), "must not contain a colon")

  # Note escapes the newlines it is allowed to hold, and is unaffected.
  text <- as.character(guanoCreate(Note="first\nsecond"))
  expect_true(grepl("Note:first\\nsecond", text, fixed=TRUE))
  expect_identical(guanoParse(text)[["Note"]], "first\nsecond")
})

test_that("Arguments that name one thing are refused several", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  expect_error(guanoCreate(Make="x", version=c("1.0", "2.0")), "single string")
  expect_error(guanoWrite(guanoCreate(Make="x"), f, output=c(tempfile(), tempfile())), "single string")
  expect_error(guanoWrite(guanoCreate(Make="x"), c(f, f)), "single string")
})

test_that("A position is a latitude and a longitude, or it is text", {
  position <- function(value) {
    suppressWarnings(
      guanoParse(paste0("GUANO|Version:1.0\nLoc Position:", value))[["Loc Position"]]
    )
  }
  expect_equal(position("51.496401 -0.122456"), c(51.496401, -0.122456))

  # Anything else is not a position. Reading it as numbers would accept it and
  # alter it, writing a third number of 99 back as 99.0.
  expect_identical(position("51.5"), "51.5")
  expect_identical(position("51.5 -0.1 99"), "51.5 -0.1 99")
  expect_identical(position(""), "")
  expect_warning(
    guanoParse("GUANO|Version:1.0\nLoc Position:51.5"),
    "latitude and a longitude"
  )

  # What is kept as text is written back as the text it was.
  g <- suppressWarnings(guanoParse("GUANO|Version:1.0\nLoc Position:51.5 -0.1 99"))
  expect_true(grepl("Loc Position:51.5 -0.1 99", as.character(g), fixed=TRUE))
})
