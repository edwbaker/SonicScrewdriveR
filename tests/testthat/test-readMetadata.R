test_that("Every format a wave file carries is read in one reading", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("guan", charToRaw("GUANO|Version:1.0\nMake:Open Acoustic Devices")),
    mdChunk("wamd", c(mdWamdField(1, "SM3"), mdWamdField(2, "SM300123"))),
    mdBextChunk("Acinipe calabra; Female song"),
    mdChunk("iXML", charToRaw("<BWFXML><PROJECT>Urban Nature</PROJECT></BWFXML>")),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Name"))),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_s3_class(m, "audioMetadata")
  expect_identical(m$container, "WAVE")
  expect_identical(
    names(m$sources), c("guano", "wamd", "bext", "ixml", "wavInfo")
  )
})

# The same fact is written under a different name by every format, so a reading
# that only handed back each format would leave the caller to work out which
# of them to believe.
test_that("The formats are merged into one record", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("wamd", c(
      mdWamdField(1, "SM3"),
      mdWamdField(2, "SM300123"),
      mdWamdField(5, "2015-10-28 20:21:13-04:00"),
      mdWamdField(6, "WGS84,42.1234N,71.5678W,100m")
    )),
    mdBextChunk("Acinipe calabra; Female song", "Orthoptera Library"),
    mdChunk("iXML", charToRaw("<BWFXML><TAKE>003</TAKE></BWFXML>")),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(m$fields$make, "Wildlife Acoustics")
  expect_identical(m$fields$model, "SM3")
  expect_identical(m$fields$serial, "SM300123")
  expect_identical(m$fields$title, "Acinipe calabra; Female song")
  expect_identical(m$fields$recordist, "Orthoptera Library")
  expect_identical(m$fields$take, "003")
  expect_equal(m$fields$location, c(latitude=42.1234, longitude=-71.5678))
  expect_equal(m$fields$elevation, 100)
  expect_identical(
    format(m$fields$datetime, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2015-10-29T00:21:13Z"
  )
})

# A format written for bioacoustics is trusted over one written for broadcast.
test_that("Where two formats disagree, the more specific one is believed", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("guan", charToRaw("GUANO|Version:1.0\nTimestamp:2020-11-03T11:24:30Z")),
    mdBextChunk("A recording", date="2011-05-23", time="14:22:03"),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(
    format(m$fields$datetime, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2020-11-03T11:24:30Z"
  )
  # Nothing is thrown away by the merge: what bext said is still there.
  expect_identical(m$sources$bext[["OriginationDate"]], "2011-05-23")
})

test_that("The record of an MP3 recording is read from its tag", {
  comment <- c(
    as.raw(0), charToRaw("eng"), as.raw(0),
    charToRaw("XC428067 Pacific Swallow "), as.raw(c(0xc2, 0xa9))
  )
  f <- mdFile(c(mdId3Tag(c(
    mdTextFrame("TIT2", "Pacific Swallow (Hirundo tahitica)"),
    mdTextFrame("TPE1", "A Recordist"),
    mdTextFrame("TDRC", "2018-02-14T06:30:00"),
    mdFrame24("COMM", comment),
    mdFrame24("WCOP", charToRaw("//creativecommons.org/licenses/by-nc-sa/4.0/")),
    mdFrame24("WOAF", charToRaw("https://www.xeno-canto.org/428067"))
  )), mdMp3Frame()), ".mp3")
  m <- readMetadata(f, filename=FALSE)

  expect_identical(m$container, "MP3")
  expect_identical(names(m$sources), "id3")
  expect_identical(m$fields$title, "Pacific Swallow (Hirundo tahitica)")
  expect_identical(m$fields$recordist, "A Recordist")
  expect_identical(m$fields$comment, "XC428067 Pacific Swallow ©")
  expect_identical(m$fields$licence, "//creativecommons.org/licenses/by-nc-sa/4.0/")
  expect_identical(m$fields$url, "https://www.xeno-canto.org/428067")
  expect_identical(
    format(m$fields$datetime, "%Y-%m-%d %H:%M:%S"), "2018-02-14 06:30:00"
  )
})

test_that("The record of a FLAC recording is read from its comments", {
  f <- mdFile(mdFlac(mdVorbisBlock("libFLAC", c(
    "TITLE=Hirundo tahitica",
    "ARTIST=A Recordist",
    "DATE=2018-02-14",
    "SPECIES=Hirundo tahitica",
    "LICENSE=CC BY-NC-SA 4.0"
  ))), ".flac")
  m <- readMetadata(f, filename=FALSE)

  expect_identical(m$container, "FLAC")
  expect_identical(names(m$sources), "vorbisComment")
  expect_identical(m$fields$title, "Hirundo tahitica")
  expect_identical(m$fields$species, "Hirundo tahitica")
  expect_identical(m$fields$licence, "CC BY-NC-SA 4.0")
})

# The file is identified by its bytes, not by its name. The collections this
# was written against hold MP3 and Ogg files under a .wav extension.
test_that("A file is read as what it is, not as what it is named", {
  f <- mdFile(c(mdId3Tag(mdTextFrame("TIT2", "An MP3 called .wav")), mdMp3Frame()))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(m$container, "MP3")
  expect_identical(m$fields$title, "An MP3 called .wav")
})

# Knowing that a file carries metadata this package cannot read is worth more
# than passing over it in silence.
test_that("Chunks that carry metadata this package cannot read are named", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Name"))),
    mdChunk("SMED", raw(8)),
    mdChunk("smpl", raw(8)),
    mdChunk("JUNK", raw(8)),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(m$unparsed, c("SMED", "smpl"))
})

# The two that were named here before now have readers of their own, so a file
# carrying them is not a file carrying metadata nothing can read.
test_that("A chunk that now has a reader is not named as one without", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("_PMX", charToRaw("<x:xmpmeta x:xmptk=\"1.0\"/>")),
    mdChunk("DISP", c(mdU32(1), charToRaw("A Display Name"))),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_length(m$unparsed, 0)
  expect_identical(m$fields$title, "A Display Name")
})

test_that("Species are taken from a format that names them, and not from a title", {
  guano <- paste(
    "GUANO|Version:1.0",
    "Species Manual ID:Pipistrellus pipistrellus",
    sep="\n"
  )
  f <- mdFile(mdWav(mdFmt(), mdChunk("guan", charToRaw(guano)), mdData()))

  expect_identical(readMetadata(f, filename=FALSE)$fields$species,
                   "Pipistrellus pipistrellus")

  titled <- mdFile(mdWav(
    mdFmt(), mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "Hirundo tahitica"))),
    mdData()
  ))
  expect_null(readMetadata(titled, filename=FALSE)$fields$species)
})

# A field holding only a year has no month or day to make a time with, and
# giving it one would invent a date the file never carried.
test_that("A year on its own gives no time", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("INFO"), mdInfoField("ICRD", "2018")
  )), mdData()))
  m <- readMetadata(f, filename=FALSE)

  expect_null(m$fields$datetime)
  expect_identical(m$sources$wavInfo[["ICRD"]], "2018")
})

test_that("A file carrying nothing gives an empty reading rather than an error", {
  f <- mdFile(mdWav(mdFmt(), mdData()))
  m <- readMetadata(f, filename=FALSE)

  expect_s3_class(m, "audioMetadata")
  expect_identical(m$container, "WAVE")
  expect_length(m$fields, 0)
  expect_length(m$sources, 0)
  expect_output(print(m), "<no metadata>")
})

test_that("A file that is not audio at all gives an empty reading", {
  f <- mdFile(charToRaw("this is not audio, not even slightly"), ".wav")
  m <- readMetadata(f, filename=FALSE)

  expect_true(is.na(m$container))
  expect_length(m$sources, 0)
})

test_that("A reading is given back as it was, and a missing file is reported", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(readMetadata(m), m)
  expect_error(readMetadata("no-such-file.wav"), "File does not exist")
  expect_error(readMetadata(42), "must be a file path")
})

test_that("The bytes of a file are read as the file is", {
  bytes <- mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData())
  f <- mdFile(bytes)

  expect_identical(readMetadata(bytes)$fields, readMetadata(f, filename=FALSE)$fields)
})

test_that("A reading can be attached to a Wave object and read back", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)
  audioMetadata(wave) <- readMetadata(f, filename=FALSE)

  expect_identical(readMetadata(wave)$fields$model, "SM3")
  audioMetadata(wave) <- NULL
  expect_length(readMetadata(wave)$sources, 0)
})

# A TaggedWave has a metadata slot made for this, so that is where its metadata
# goes, rather than into a second place of this package's own invention.
test_that("A TaggedWave keeps the reading in its metadata slot", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  tagged <- tagWave(tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16))

  audioMetadata(tagged) <- readMetadata(f, filename=FALSE)

  expect_s3_class(tagged@metadata[["audioMetadata"]], "audioMetadata")
  expect_null(attr(tagged, "audioMetadata"))
  expect_identical(readMetadata(tagged)$fields$model, "SM3")
  expect_true(methods::validObject(tagged))
})

# The slot is a list, and the reading goes in under a name of its own, so
# whatever the caller has recorded there is left as it was.
test_that("A reading sits beside what the caller put in the slot", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  tagged <- tagWave(tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16))
  tagged@metadata <- list(site="a hedgerow")

  audioMetadata(tagged) <- readMetadata(f, filename=FALSE)

  expect_identical(tagged@metadata[["site"]], "a hedgerow")
  expect_identical(readMetadata(tagged)$fields$model, "SM3")

  audioMetadata(tagged) <- NULL
  expect_identical(tagged@metadata, list(site="a hedgerow"))
  expect_true(methods::validObject(tagged))
})

test_that("A TaggedWaveMC keeps the reading in its slot as well", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  channels <- matrix(rep(0, 20), ncol=2, dimnames=list(NULL, c("FL", "FR")))
  tagged <- tagWave(tuneR::WaveMC(channels, samp.rate=48000, bit=16))

  audioMetadata(tagged) <- readMetadata(f, filename=FALSE)

  expect_s3_class(tagged@metadata[["audioMetadata"]], "audioMetadata")
  expect_identical(readMetadata(tagged)$fields$model, "SM3")
  expect_true(methods::validObject(tagged))
})

# A plain Wave has no slot, so it uses an attribute of the same name.
test_that("A plain Wave keeps the reading as an attribute", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)

  audioMetadata(wave) <- readMetadata(f, filename=FALSE)

  expect_s3_class(attr(wave, "audioMetadata"), "audioMetadata")
  expect_identical(readMetadata(wave)$fields$model, "SM3")
})

# GUANO is one format among the others rather than a store of its own, so
# setting it leaves the rest of what the wave says about its file alone.
test_that("GUANO is kept in the same place as every other format", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)
  audioMetadata(wave) <- readMetadata(f, filename=FALSE)

  guano(wave) <- guanoCreate(Make="Open Acoustic Devices", Model="AudioMoth")

  expect_null(attr(wave, "guano"))
  expect_identical(guano(wave)[["Model"]], "AudioMoth")
  expect_identical(
    sort(names(readMetadata(wave)$sources)), c("guano", "wamd")
  )
  # GUANO is the more specific format, so it is the one believed.
  expect_identical(readMetadata(wave)$fields$model, "AudioMoth")
  expect_identical(readMetadata(wave)$sources$wamd[["model"]], "SM3")
})

test_that("GUANO set on a tagged wave goes into its slot too", {
  tagged <- tagWave(tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16))

  guano(tagged) <- guanoCreate(Make="Open Acoustic Devices")

  expect_s3_class(tagged@metadata[["audioMetadata"]], "audioMetadata")
  expect_identical(guano(tagged)[["Make"]], "Open Acoustic Devices")
  expect_true(methods::validObject(tagged))
})

test_that("Removing the only format a wave carried leaves it carrying nothing", {
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)
  guano(wave) <- guanoCreate(Make="Open Acoustic Devices")

  guano(wave) <- NULL

  expect_null(attr(wave, "audioMetadata"))
  expect_length(guano(wave), 0)
  expect_length(readMetadata(wave)$sources, 0)
})

test_that("Metadata can only be attached to a wave, and only as a reading", {
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)

  expect_error(audioMetadata(wave) <- "not a reading", "must be an audioMetadata object")
  notAWave <- 1
  expect_error(audioMetadata(notAWave) <- NULL, "can only be attached to a Wave")
})

# The filename is the one source nothing outranks, so where the file said
# nothing a guess from its name would be the whole answer. A collection that
# numbers its records gives names of the same shape as a date or a count of
# seconds, and a confident wrong date is worse than no date at all.
test_that("A date the filename left in doubt is not reported", {
  bytes <- mdWav(mdFmt(), mdData())
  for (name in c("20190101.wav", "1234567890.wav", "12345678.wav")) {
    path <- file.path(tempdir(), name)
    on.exit(unlink(path), add=TRUE)
    writeBin(bytes, path)
    m <- readMetadata(path)

    expect_null(m$fields$datetime)
    # What the name said is still there to be looked at.
    expect_true(isTRUE(m$sources$filename$ambiguous))
  }
})

# A caller who knows what their own files are named by can say so, and then the
# digits are not a guess any more.
test_that("Naming the filename format settles a name the package could not", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "20190101.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)

  expect_null(readMetadata(path)$fields$datetime)
  expect_identical(
    format(readMetadata(path, filename="YYYYMMDD")$fields$datetime, "%Y-%m-%d"),
    "2019-01-01"
  )
  expect_false("ambiguous" %in% names(
    readMetadata(path, filename="YYYYMMDD")$sources$filename
  ))
})

test_that("A count of seconds can be named too", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "1234567890.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)

  expect_null(readMetadata(path)$fields$datetime)
  expect_identical(
    as.numeric(readMetadata(path, filename="timestamp")$fields$datetime),
    1234567890
  )
})

# A name that spells no real date gives no date even when it was asked for as
# one, rather than a date that is missing.
test_that("Naming a format does not make a name into a date it cannot be", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "12345678.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)

  expect_null(readMetadata(path, filename="YYYYMMDD")$fields$datetime)
})

test_that("A filename format this package does not know is refused at once", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "20190101.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)

  expect_error(readMetadata(path, filename="YYYY-MM-DD"), "Unknown filename format")
  expect_error(readMetadata(path, filename="match"), "Unknown filename format")
  expect_error(readMetadata(path, filename=c("YYYYMMDD", "timestamp")), "must be TRUE")
  # The format is checked before the file is, so the complaint is about the
  # format whatever the file turns out to be.
  expect_error(readMetadata("no-such-file.wav", filename="nonsense"), "Unknown filename format")
})

test_that("A name that says plainly what it is still gives its date", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "20240220_162231.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)

  expect_identical(
    format(readMetadata(path)$fields$datetime, "%Y-%m-%d %H:%M:%S"),
    "2024-02-20 16:22:31"
  )
})

# Much of the TSA corpus holds no embedded metadata at all, so for those
# recordings the name is the only record of what was recorded.
test_that("A TSA name gives the species and collection of a file holding nothing", {
  bytes <- mdWav(mdFmt(), mdData())
  path <- file.path(tempdir(), "TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)
  m <- readMetadata(path)

  expect_identical(m$fields$species, "Porzana parva")
  expect_identical(m$fields$collection, "TSA")
  expect_null(m$fields$datetime)
  expect_identical(names(m$sources), "filename")
})

# A name is read only where the file itself did not say, so a recording that
# carries its own species keeps it.
test_that("What the file says about its species outranks its name", {
  guano <- paste(
    "GUANO|Version:1.0",
    "Species Manual ID:Pipistrellus pipistrellus",
    sep="\n"
  )
  bytes <- mdWav(mdFmt(), mdChunk("guan", charToRaw(guano)), mdData())
  path <- file.path(tempdir(), "TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")
  on.exit(unlink(path), add=TRUE)
  writeBin(bytes, path)
  m <- readMetadata(path)

  expect_identical(m$fields$species, "Pipistrellus pipistrellus")
  expect_identical(m$sources$filename$taxon, "Porzana parva")
})

test_that("What a filename says is read, and can be left unread", {
  bytes <- mdWav(mdFmt(), mdData())
  named <- file.path(tempdir(), "PREFIX_20210712_043000.wav")
  on.exit(unlink(named), add=TRUE)
  writeBin(bytes, named)

  m <- readMetadata(named)
  expect_true("filename" %in% names(m$sources))
  expect_identical(format(m$fields$datetime, "%Y-%m-%d %H:%M:%S"), "2021-07-12 04:30:00")
  expect_identical(m$fields$prefix, "PREFIX")

  expect_false("filename" %in% names(readMetadata(named, filename=FALSE)$sources))
})

# What is inside the file is trusted over what its name says: a name is a
# convention that a rename or a download can break.
test_that("The contents of a file are believed over its name", {
  bytes <- mdWav(
    mdFmt(),
    mdChunk("guan", charToRaw("GUANO|Version:1.0\nTimestamp:2020-11-03T11:24:30Z")),
    mdData()
  )
  named <- file.path(tempdir(), "PREFIX_20210712_043000.wav")
  on.exit(unlink(named), add=TRUE)
  writeBin(bytes, named)

  expect_identical(
    format(readMetadata(named)$fields$datetime, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    "2020-11-03T11:24:30Z"
  )
})

test_that("A name that says nothing this package recognises is not an error", {
  bytes <- mdWav(mdFmt(), mdData())
  named <- file.path(tempdir(), "nothing-in-this-name.wav")
  on.exit(unlink(named), add=TRUE)
  writeBin(bytes, named)

  expect_length(readMetadata(named)$sources, 0)
})

# A name of eight digits is read as a date, and one that spells no real date
# leaves a time that is missing rather than no time at all. Reporting it would
# put a field in the record saying the recording has a date and that the date
# is not known, which is a worse answer than not mentioning one.
test_that("A name of digits that spell no date gives no date", {
  bytes <- mdWav(mdFmt(), mdData())
  named <- file.path(tempdir(), "13131313.wav")
  on.exit(unlink(named), add=TRUE)
  writeBin(bytes, named)
  m <- readMetadata(named)

  expect_null(m$fields$datetime)
  expect_false("datetime" %in% names(m$fields))
})

# ------------------------------------------------- the files the package ships --

test_that("The bundled AudioMoth recording is read from its comment", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  m <- readMetadata(f)

  expect_identical(m$container, "WAVE")
  expect_true(all(c("audiomoth", "wavInfo") %in% names(m$sources)))
  expect_identical(m$fields$make, "Open Acoustic Devices")
  expect_identical(m$fields$model, "AudioMoth")
  expect_identical(m$fields$serial, "240435055C7B17DB")
  expect_identical(
    format(m$fields$datetime, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2021-05-01T23:32:00Z"
  )
})

test_that("The bundled GUANO recording is read from its GUANO metadata", {
  f <- system.file("extdata/AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
  m <- readMetadata(f)

  expect_true("guano" %in% names(m$sources))
  expect_identical(m$fields$make, "Open Acoustic Devices")
  expect_identical(m$fields$model, "AudioMoth")
  expect_identical(m$fields$firmware, "AudioMoth-Firmware-Basic (1.11.0)")
  expect_equal(m$fields$location, c(latitude=51.4964, longitude=-0.1224))
  expect_identical(
    format(m$fields$datetime, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), "2026-05-14T20:06:00Z"
  )
})
