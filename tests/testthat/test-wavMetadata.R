test_that("The fields of an INFO list are read by their own identifiers", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("INFO"),
    mdInfoField("INAM", "Pacific Swallow"),
    mdInfoField("IART", "A Recordist"),
    mdInfoField("ICMT", "calling from a wire"),
    mdInfoField("ICRD", "2026-05-14"),
    mdInfoField("ISFT", "Some Editor 2.1"),
    mdInfoField("ICOP", "CC BY-NC-SA 4.0")
  )), mdData()))
  i <- wavInfo(f)

  expect_s3_class(i, "wavInfo")
  expect_identical(i[["INAM"]], "Pacific Swallow")
  expect_identical(i[["IART"]], "A Recordist")
  expect_identical(i[["ICMT"]], "calling from a wire")
  expect_identical(i[["ICRD"]], "2026-05-14")
  expect_identical(i[["ISFT"]], "Some Editor 2.1")
  expect_identical(i[["ICOP"]], "CC BY-NC-SA 4.0")
})

# A cue label says nothing about the recording as a whole, so it is named by
# its list as well and cannot be mistaken for something that does.
test_that("A list that is not one of information is named by its kind", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("adtl"), mdCueField("labl", 3L, "a cue label")
  )), mdData()))

  expect_identical(wavInfo(f)[["adtl/labl"]], "a cue label")
})

# The fields of an associated data list begin with the identifier of the cue
# point they belong to, and a labelled text field with a header of sample
# counts and language codes besides. Reading those as text put a run of nuls
# and mojibake in front of every label, which is how real files in the corpora
# this was written against turned up.
test_that("The cue point a label belongs to is not read as part of it", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("adtl"),
    mdCueField("labl", 3L, "Cue 3"),
    mdCueField("note", 7L, "Desc."),
    mdCueField("ltxt", 1L, "rgn", header=20L)
  )), mdData()))
  i <- wavInfo(f)

  expect_identical(i[["adtl/labl"]], "Cue 3")
  expect_identical(i[["adtl/note"]], "Desc.")
  expect_identical(i[["adtl/ltxt"]], "rgn")
})

test_that("A label field holding nothing but its cue point is not reported", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("adtl"),
    mdCueField("labl", 3L, ""),
    mdInfoField("INAM", "A Name")
  )), mdData()))

  expect_false("adtl/labl" %in% names(wavInfo(f)))
})

test_that("Several labels in one list are all kept", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("adtl"),
    mdCueField("labl", 1L, "Cue 3"),
    mdCueField("labl", 2L, "Phrase 1")
  )), mdData()))
  i <- wavInfo(f)

  expect_identical(i[["adtl/labl"]], "Cue 3")
  expect_identical(i[["adtl/labl.2"]], "Phrase 1")
})

# An INFO field holds nothing but text, so nothing is passed over there.
test_that("A field of an INFO list keeps its first four bytes", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("INFO"), mdInfoField("ICMT", "Recorded well")
  )), mdData()))

  expect_identical(wavInfo(f)[["ICMT"]], "Recorded well")
})

# A file may hold a list of cue labels and a list of information, in either
# order. Taking the first and looking no further would miss the other.
test_that("Every list in the file is read, whichever comes first", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("LIST", c(charToRaw("adtl"), mdCueField("labl", 1L, "a cue label"))),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Name"))),
    mdData()
  ))
  i <- wavInfo(f)

  expect_identical(i[["adtl/labl"]], "a cue label")
  expect_identical(i[["INAM"]], "A Name")
})

test_that("A file with no list gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(wavInfo(f), "wavInfo")
  expect_length(wavInfo(f), 0)
})

test_that("A field declaring more bytes than the list holds ends the reading", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", c(
    charToRaw("INFO"),
    mdInfoField("INAM", "A Name"),
    charToRaw("ICMT"), mdU32(5000), charToRaw("short")
  )), mdData()))

  expect_identical(names(wavInfo(f)), "INAM")
})

# ---------------------------------------------------------------------- iXML --

mdIxmlText <- function() {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><BWFXML>",
    "<PROJECT>Urban Nature</PROJECT>",
    "<TAKE>003</TAKE>",
    "<TAPE>A&amp;B</TAPE>",
    "<NOTE>wind &lt;5 knots, &#176;C falling</NOTE>",
    "<SPEED><NOTE>48000</NOTE></SPEED>",
    "<TRACK_LIST COUNT=\"2\">",
    "<TRACK><NAME>Left</NAME></TRACK><TRACK><NAME>Right</NAME></TRACK>",
    "</TRACK_LIST>",
    "</BWFXML>"
  )
}

test_that("The elements of an iXML chunk are read by their own tags", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", charToRaw(mdIxmlText())), mdData()))
  x <- ixml(f)

  expect_s3_class(x, "ixml")
  expect_identical(x[["PROJECT"]], "Urban Nature")
  expect_identical(x[["TAKE"]], "003")
})

test_that("The escaping that XML puts on a value is undone", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", charToRaw(mdIxmlText())), mdData()))
  x <- ixml(f)

  expect_identical(x[["TAPE"]], "A&B")
  expect_identical(x[["NOTE"]], "wind <5 knots, °C falling")
})

# An element holding other elements holds no value of its own, so its leaves
# are read instead of the run of text they would spell together.
test_that("An element holding other elements is passed over for its leaves", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", charToRaw(mdIxmlText())), mdData()))
  x <- ixml(f)

  expect_false("SPEED" %in% names(x))
  expect_false("TRACK_LIST" %in% names(x))
  expect_identical(x[["NOTE.2"]], "48000")
})

test_that("A tag used more than once keeps every value it was used for", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", charToRaw(mdIxmlText())), mdData()))
  x <- ixml(f)

  expect_identical(x[["NAME"]], "Left")
  expect_identical(x[["NAME.2"]], "Right")
})

test_that("The document itself is kept on the result", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", charToRaw(mdIxmlText())), mdData()))

  expect_match(attr(ixml(f), "xml"), "^<\\?xml")
})

test_that("A file with no iXML chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(ixml(f), "ixml")
  expect_length(ixml(f), 0)
})

# --------------------------------------------- reading from bytes and waves ---

test_that("Every reader reads the bytes of a file as it reads the file", {
  bytes <- mdWav(
    mdFmt(),
    mdChunk("wamd", mdWamdField(1, "SM3")),
    mdBextChunk("A recording"),
    mdChunk("iXML", charToRaw(mdIxmlText())),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Name"))),
    mdData()
  )
  f <- mdFile(bytes)

  expect_identical(unclass(wamd(bytes)), unclass(wamd(f)))
  expect_identical(unclass(bext(bytes)), unclass(bext(f)))
  expect_identical(unclass(ixml(bytes)), unclass(ixml(f)))
  expect_identical(unclass(wavInfo(bytes)), unclass(wavInfo(f)))
})

test_that("A reader given something that is not audio gives an empty result", {
  f <- mdFile(charToRaw("this is not audio, not even slightly"), ".wav")

  expect_length(wamd(f), 0)
  expect_length(bext(f), 0)
  expect_length(ixml(f), 0)
  expect_length(wavInfo(f), 0)
  expect_length(id3(f), 0)
  expect_length(vorbisComment(f), 0)
})

test_that("A reader given a file that does not exist says so", {
  expect_error(wamd("no-such-file.wav"), "File does not exist")
  expect_error(bext(42), "must be a file path")
})

# A directory passes file.exists(), and opening one fails with a message about
# permissions that says nothing about what was actually wrong.
test_that("A reader given a directory says that is what it was given", {
  expect_error(wamd(tempdir()), "is a directory")
  expect_error(readMetadata(tempdir()), "is a directory")
})

test_that("A container this package reads no metadata from is still named", {
  rf64 <- c(
    charToRaw("RF64"), mdU32(4294967295), charToRaw("WAVE"), mdFmt(), mdData()
  )
  aiff <- c(charToRaw("FORM"), mdU32(100), charToRaw("AIFF"), raw(50))

  expect_identical(readMetadata(mdFile(rf64), filename=FALSE)$container, "RF64")
  expect_identical(readMetadata(mdFile(aiff, ".aiff"), filename=FALSE)$container, "AIFF")
  expect_length(readMetadata(mdFile(rf64), filename=FALSE)$sources, 0)
})

test_that("An empty file is read as carrying nothing, not as an error", {
  f <- mdFile(raw(0))

  expect_true(is.na(readMetadata(f, filename=FALSE)$container))
  expect_length(wamd(f), 0)
  expect_length(id3(f), 0)
})

# A chunk that declares a size no machine could hold, which a file damaged in
# the right place does, must not be read into memory.
test_that("A chunk declaring an absurd size is not read", {
  f <- mdFile(c(
    charToRaw("RIFF"), mdU32(40), charToRaw("WAVE"),
    charToRaw("wamd"), mdU32(4000000000), charToRaw("SM3")
  ))

  expect_length(wamd(f), 0)
})

test_that("A reader given what it has already read gives it back", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  w <- wamd(f)

  expect_identical(wamd(w), w)
})

test_that("A reader reads the metadata a Wave object carries", {
  wave <- tuneR::Wave(left=rep(0, 10), samp.rate=48000, bit=16)
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", mdWamdField(1, "SM3")), mdData()))
  audioMetadata(wave) <- readMetadata(f, filename=FALSE)

  expect_identical(wamd(wave)[["model"]], "SM3")
  expect_length(bext(wave), 0)
})

test_that("Printing says which format gave nothing", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_output(print(wamd(f)), "<no wamd metadata>")
  expect_output(print(bext(f)), "<no bext metadata>")
  expect_output(print(wavInfo(f)), "<no INFO metadata>")
  expect_output(print(ixml(f)), "<no iXML metadata>")
})

test_that("A reading can be formatted a field to a line", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(1, "SM3"), mdWamdField(2, "SM300123")
  )), mdData()))

  expect_identical(format(wamd(f)), c("model: SM3", "serial: SM300123"))
})

test_that("Taking some of the fields keeps the class", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("wamd", c(
    mdWamdField(1, "SM3"), mdWamdField(2, "SM300123")
  )), mdData()))

  expect_s3_class(wamd(f)["model"], "wamd")
})
