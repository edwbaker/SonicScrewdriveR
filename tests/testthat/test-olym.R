# The olym chunk of a wave file: what an Olympus recorder wrote about the
# recording. The chunk is a fixed layout, so a reader of it is a set of
# offsets, and what these tests are mostly about is the difference between a
# field a recorder wrote and a field it left as padding. The recorders pad with
# 0xff where other formats pad with nothing, and a reader that took that for a
# value would report a model, a time or a run of text for a file that gave
# none.

test_that("The fields an Olympus recorder wrote are read", {
  f <- mdFile(mdOlymWav(mdOlymChunk()))
  o <- olym(f)

  expect_s3_class(o, "olym")
  expect_identical(o[["model"]], "LS-100")
  expect_identical(o[["fileNumber"]], 204)
  expect_identical(o[["timestamp"]], as.POSIXct("2012-05-29 09:28:19", tz=""))
  expect_identical(o[["timestampEnd"]], as.POSIXct("2012-05-29 09:29:53", tz=""))
  expect_identical(o[["recordingTime"]], 90)
  expect_identical(o[["duration"]], 90.563)
})

# The two forms of the chunk hold the duration in different places, and each
# leaves 0xff where the other writes. Reading only one offset would give no
# duration at all for half the files that have one.
test_that("The duration is read from wherever the form of the chunk holds it", {
  new <- olym(mdFile(mdOlymWav(mdOlymChunk(form="wav", duration=33434))))
  old <- olym(mdFile(mdOlymWav(mdOlymChunk(form="dss", duration=686764))))

  expect_identical(new[["duration"]], 33.434)
  expect_identical(old[["duration"]], 686.764)
})

test_that("A chunk holding no duration in either place gives none", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(duration=NULL))))

  expect_false("duration" %in% names(o))
  expect_identical(o[["recordingTime"]], 90)
})

test_that("The length of the recording is read as seconds", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(recordingTime="011529"))))

  expect_identical(o[["recordingTime"]], 4529)
})

# ------------------------------------------------------------- the clock ---

# The year is two digits, so the century is a convention rather than something
# the file says. Olympus firmware reads a year under 70 as this century.
test_that("A year under seventy is read as this century", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(start="690101000000"))))

  expect_identical(format(o[["timestamp"]], "%Y"), "2069")
})

test_that("A year of seventy or more is read as the last century", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(start="700101000000"))))

  expect_identical(format(o[["timestamp"]], "%Y"), "1970")
})

# This is what a recorder whose clock was never set writes, and it is the
# common case in the corpora for the fields a recorder has nothing for.
test_that("A time field left as padding gives no time", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(start=NULL, end=NULL))))

  expect_false("timestamp" %in% names(o))
  expect_false("timestampEnd" %in% names(o))
  expect_identical(o[["model"]], "LS-100")
})

test_that("A time field holding something that is not twelve digits gives no time", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(start="2012-05-29 09"))))

  expect_false("timestamp" %in% names(o))
})

test_that("A date that is no date gives no time rather than an error", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(start="129929092819"))))

  expect_false("timestamp" %in% names(o))
})

test_that("A length of recording that is not six digits gives none", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(recordingTime="ff:ff"))))

  expect_false("recordingTime" %in% names(o))
})

test_that("A file number left as padding gives none", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(fileNumber=NULL))))

  expect_false("fileNumber" %in% names(o))
})

# ----------------------------------------------------------- the padding ---

# The older form fills the first text field with 0xff as far as 0x88 and with
# zeroes after it. Stripping the nuls and trimming the spaces, which is what
# every other reader here does, leaves a run of 255s: before this was dealt
# with, all 217 files of that form in the corpora reported a field of them.
test_that("A text field padded with 0xff and then nuls holds nothing", {
  # The pattern the older form writes, as a field of its own.
  padded <- as.raw(c(rep(255, 0x89), rep(0, 972 - 0x89)))[0x6a + seq_len(100)]
  expect_null(.olymText(padded))

  # And through the reader, where the chunk is padded with 0xff throughout.
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(form="dss"))))
  expect_false("text" %in% names(o))
  expect_false("text.2" %in% names(o))
})

test_that("A text field of nothing but spaces holds nothing", {
  expect_null(.olymText(as.raw(rep(32, 100))))
})

test_that("A text field of nothing but zeroes holds nothing", {
  expect_null(.olymText(raw(100)))
})

test_that("A text field that is not there at all holds nothing", {
  expect_null(.olymText(raw(0)))
})

# Padding is dropped from the ends rather than wherever it appears, so that a
# field whose text happens to hold the character 0xff keeps it.
test_that("Text keeps a character that the padding is also made of", {
  bytes <- c(as.raw(rep(255, 4)), charToRaw("Ma"), as.raw(255),
             charToRaw("do"), as.raw(rep(0, 4)))

  expect_identical(.olymText(bytes), "Maÿdo")
})

# ------------------------------------------------------------- the marks ---

test_that("The marks a recorder was told to put in a recording are read", {
  o <- olym(mdFile(mdOlymWav(
    mdOlymChunk(form="dss", marks=c(3152, 24825, 93044))
  )))

  expect_identical(o[["marks"]], c(3.152, 24.825, 93.044))
})

test_that("A recording with no marks gives none", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(form="dss"))))

  expect_false("marks" %in% names(o))
})

# Four of the nine files in the corpora that carry a mark hold a mark of zero,
# which cannot be told from a slot a firmware filled with zero rather than
# with 0xffffffff. It is read as the mark it is written as.
test_that("A mark at the very start of a recording is read as a mark", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(form="dss", marks=c(0, 1500)))))

  expect_identical(o[["marks"]], c(0, 1.5))
})

test_that("Only the slots that were filled are read", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(form="dss", marks=c(500, 2500)))))

  expect_length(o[["marks"]], 2)
})

# -------------------------------------------------------------- the text ---

test_that("Text a recorder wrote is read from the slot it was written in", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(text="Bukit Timah", text2="Katydids"))))

  expect_identical(o[["text"]], "Bukit Timah")
  expect_identical(o[["text.2"]], "Katydids")
})

# The one file of the 397 that writes in either slot leaves the first blank.
# The second has to keep its own name, or a field read from one slot would be
# reported under the name of the other.
test_that("Text in the second slot alone keeps the name of that slot", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(text=NULL, text2="Katydids"))))

  expect_false("text" %in% names(o))
  expect_identical(o[["text.2"]], "Katydids")
})

# ------------------------------------------------------------ the chunk ----

test_that("A chunk too short to hold a field is read as far as it goes", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(size=0x40))))

  expect_identical(o[["model"]], "LS-100")
  expect_identical(o[["timestamp"]], as.POSIXct("2012-05-29 09:28:19", tz=""))
  expect_false("recordingTime" %in% names(o))
  expect_false("duration" %in% names(o))
  expect_false("marks" %in% names(o))
})

test_that("A chunk holding nothing but padding gives nothing", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk(
    model=NULL, fileNumber=NULL, start=NULL, end=NULL, recordingTime=NULL,
    duration=NULL
  ))))

  expect_s3_class(o, "olym")
  expect_length(o, 0)
})

test_that("A file holding more than one chunk keeps every one of them", {
  f <- mdFile(mdOlymWav(
    mdOlymChunk(model="LS-10", fileNumber=1),
    mdOlymChunk(model="LS-100", fileNumber=2)
  ))
  o <- olym(f)

  expect_identical(o[["model"]], "LS-10")
  expect_identical(o[["model.2"]], "LS-100")
  expect_identical(o[["fileNumber.2"]], 2)
})

test_that("A file with no olym chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(olym(f), "olym")
  expect_length(olym(f), 0)
})

test_that("The bytes of a file are read as the file is", {
  bytes <- mdOlymWav(mdOlymChunk())

  expect_identical(unclass(olym(bytes)), unclass(olym(mdFile(bytes))))
})

test_that("An olym object given back is the one that was given", {
  o <- olym(mdFile(mdOlymWav(mdOlymChunk())))

  expect_identical(olym(o), o)
})

test_that("A file that is not audio gives an empty result", {
  f <- mdFile(charToRaw("this is not audio, not even slightly"), ".wav")

  expect_length(olym(f), 0)
})

test_that("A file that does not exist says so", {
  expect_error(olym(file.path(tempdir(), "no-such-recording.wav")),
               "does not exist")
})

test_that("An empty result says which format it found nothing of", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_output(print(olym(f)), "<no olym metadata>")
})

# ------------------------------------------------- the normalised record ---

test_that("The recorder's time and model are the normalised record", {
  m <- readMetadata(mdFile(mdOlymWav(mdOlymChunk(model="LS-11"))),
                    filename=FALSE)

  expect_identical(m$fields$datetime, as.POSIXct("2012-05-29 09:28:19", tz=""))
  expect_identical(m$fields$model, "LS-11")
})

# The chunk is Olympus's own, so a file carrying one came off one of their
# recorders whether or not any field says so. One file in the corpora holds
# something else in the model field, and the make is still Olympus.
test_that("A file carrying the chunk is an Olympus recording whatever the model says", {
  m <- readMetadata(mdFile(mdOlymWav(mdOlymChunk(model="BERN"))),
                    filename=FALSE)

  expect_identical(m$fields$make, "Olympus")
  expect_identical(m$fields$model, "BERN")
})

# Nothing says what the text fields are for, so neither becomes the title.
test_that("Text the chunk holds is not taken for the title of the recording", {
  m <- readMetadata(mdFile(mdOlymWav(mdOlymChunk(text2="Katydids"))),
                    filename=FALSE)

  expect_null(m$fields$title)
  expect_identical(m$sources$olym[["text.2"]], "Katydids")
})

# The number is a count of files on a device, not a take of a recording.
test_that("The number the recording was given on the device is not a take", {
  m <- readMetadata(mdFile(mdOlymWav(mdOlymChunk(fileNumber=204))),
                    filename=FALSE)

  expect_null(m$fields$take)
  expect_identical(m$sources$olym[["fileNumber"]], 204)
})

# A recorder's own record of what it was and when it ran beats a general store
# a program filled in later, which is the argument wamd is read ahead of them
# by as well.
test_that("The recorder's own record outranks an INFO list", {
  m <- readMetadata(
    mdFile(mdWav(
      mdFmt(),
      mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("IPRD", "A Product"))),
      mdOlymChunk(model="LS-11"),
      mdData()
    )),
    filename=FALSE
  )

  expect_identical(m$fields$model, "LS-11")
})

test_that("An olym chunk is no longer counted among the chunks nothing reads", {
  m <- readMetadata(mdFile(mdOlymWav(mdOlymChunk())), filename=FALSE)

  expect_false("olym" %in% m$unparsed)
  expect_true("olym" %in% names(m$sources))
})

test_that("A chunk holding nothing leaves no source behind and no complaint", {
  m <- readMetadata(
    mdFile(mdOlymWav(mdOlymChunk(
      model=NULL, fileNumber=NULL, start=NULL, end=NULL, recordingTime=NULL,
      duration=NULL
    ))),
    filename=FALSE
  )

  expect_false("olym" %in% m$unparsed)
  expect_false("olym" %in% names(m$sources))
})
