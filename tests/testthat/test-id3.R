# The frames below are those xeno-canto writes into the MP3 it serves for a
# recording, which is where the whole of its record for that recording is held.
mdXenoCantoTag <- function() {
  # The comment frame is declared as ISO-8859-1 and holds UTF-8 bytes: the two
  # bytes c2 a9 are a copyright sign in UTF-8 and two characters in
  # ISO-8859-1, so which encoding is used decides what comes back.
  comment <- c(
    as.raw(0), charToRaw("eng"), as.raw(0),
    charToRaw("XC428067 Pacific Swallow "), as.raw(c(0xc2, 0xa9)),
    charToRaw(" A Recordist")
  )
  return(mdId3Tag(c(
    mdTextFrame("TIT2", "Pacific Swallow (Hirundo tahitica)"),
    mdTextFrame("TPE1", "A Recordist"),
    mdTextFrame("TCON", "Hirundinidae"),
    mdTextFrame("TDRC", "2018-02-14T06:30:00"),
    mdFrame24("COMM", comment),
    mdFrame24("WCOP", charToRaw("//creativecommons.org/licenses/by-nc-sa/4.0/")),
    mdFrame24("WOAF", charToRaw("https://www.xeno-canto.org/428067")),
    mdFrame24("APIC", as.raw(c(0, 1, 2, 3, 4, 5))),
    raw(32)
  )))
}

test_that("A version 2.4 tag is read frame by frame", {
  f <- mdFile(c(mdXenoCantoTag(), mdMp3Frame()), ".mp3")
  t <- id3(f)

  expect_s3_class(t, "id3")
  expect_identical(attr(t, "version"), "2.4.0")
  expect_identical(t[["TIT2"]], "Pacific Swallow (Hirundo tahitica)")
  expect_identical(t[["TPE1"]], "A Recordist")
  expect_identical(t[["TCON"]], "Hirundinidae")
  expect_identical(t[["WCOP"]], "//creativecommons.org/licenses/by-nc-sa/4.0/")
  expect_identical(t[["WOAF"]], "https://www.xeno-canto.org/428067")
})

test_that("A comment declared as ISO-8859-1 but written as UTF-8 is read as UTF-8", {
  f <- mdFile(c(mdXenoCantoTag(), mdMp3Frame()), ".mp3")

  expect_identical(id3(f)[["COMM"]], "XC428067 Pacific Swallow © A Recordist")
})

test_that("The language of a comment is recorded, and its nul is not read as text", {
  f <- mdFile(c(mdXenoCantoTag(), mdMp3Frame()), ".mp3")
  t <- id3(f)

  expect_identical(attr(t, "languages")[["COMM"]], "eng")
  expect_false(grepl("^\\|", t[["COMM"]]))
})

# Artwork is not text, and reading it as text gives nothing worth having. That
# the file carries it is worth knowing, so its size is recorded.
test_that("A frame that does not hold text is passed over, and its size recorded", {
  f <- mdFile(c(mdXenoCantoTag(), mdMp3Frame()), ".mp3")
  t <- id3(f)

  expect_false("APIC" %in% names(t))
  expect_identical(attr(t, "skipped")[["APIC"]], 6)
})

test_that("A comment with a description is named by it", {
  comment <- c(
    as.raw(0), charToRaw("eng"), charToRaw("Remarks"), as.raw(0),
    charToRaw("calling from a wire")
  )
  f <- mdFile(mdId3Tag(mdFrame24("COMM", comment)), ".mp3")

  expect_identical(id3(f)[["COMM|Remarks"]], "calling from a wire")
})

test_that("Two frames of the same name are both kept", {
  first <- c(as.raw(0), charToRaw("eng"), as.raw(0), charToRaw("the first"))
  second <- c(as.raw(0), charToRaw("fra"), as.raw(0), charToRaw("the second"))
  f <- mdFile(mdId3Tag(c(mdFrame24("COMM", first), mdFrame24("COMM", second))), ".mp3")
  t <- id3(f)

  expect_identical(t[["COMM"]], "the first")
  expect_identical(t[["COMM.2"]], "the second")
  expect_identical(attr(t, "languages")[["COMM.2"]], "fra")
})

# Version 2.4 allows one frame to hold several values, which is how a recording
# credited to two recordists is written.
test_that("A frame holding several values gives all of them", {
  body <- c(as.raw(0), charToRaw("One"), as.raw(0), charToRaw("Two"))
  f <- mdFile(mdId3Tag(mdFrame24("TPE1", body)), ".mp3")

  expect_identical(id3(f)[["TPE1"]], c("One", "Two"))
})

test_that("A user-defined frame is named by its description", {
  body <- c(as.raw(0), charToRaw("Recordist"), as.raw(0), charToRaw("A Recordist"))
  f <- mdFile(mdId3Tag(mdFrame24("TXXX", body)), ".mp3")

  expect_identical(id3(f)[["TXXX|Recordist"]], "A Recordist")
})

# Version 2.3 writes its sizes plainly rather than seven bits to the byte, and
# puts the size of its extended header in four bytes that it does not count.
test_that("A version 2.3 tag is read, extended header and all", {
  extended <- c(mdBE32(6), raw(6))
  frames <- mdFrame23("TIT2", c(as.raw(0), charToRaw("Version 2.3")))
  f <- mdFile(c(
    charToRaw("ID3"), as.raw(c(3, 0, 64)),
    mdSyncsafe(length(extended) + length(frames)), extended, frames
  ), ".mp3")
  t <- id3(f)

  expect_identical(attr(t, "version"), "2.3.0")
  expect_identical(t[["TIT2"]], "Version 2.3")
})

test_that("A version 2.2 tag is read, with its shorter names and headers", {
  f <- mdFile(mdId3Tag(mdTextFrame("TT2", "Version 2.2"), major=2L), ".mp3")
  t <- id3(f)

  expect_identical(attr(t, "version"), "2.2.0")
  expect_identical(t[["TT2"]], "Version 2.2")
})

# A tag may be written so that nothing in it can be mistaken for the start of
# an MP3 frame, by following every 255 with a nul that is not part of the tag.
test_that("An unsynchronised tag is read with the added nuls removed", {
  frame <- mdFrame24("TIT2", c(as.raw(0), charToRaw("A"), as.raw(255), charToRaw("B")))
  unsynced <- mdUnsync(frame)
  f <- mdFile(c(
    charToRaw("ID3"), as.raw(c(4, 0, 128)), mdSyncsafe(length(unsynced)), unsynced
  ), ".mp3")

  expect_identical(length(unsynced), length(frame) + 1L)
  expect_identical(id3(f)[["TIT2"]], "AÿB")
})

test_that("A frame unsynchronised on its own is read the same way", {
  body <- c(as.raw(0), charToRaw("A"), as.raw(255), charToRaw("B"))
  # The second flag byte says the frame is unsynchronised; the size is that of
  # the frame as it is stored.
  stored <- mdUnsync(body)
  frame <- c(charToRaw("TIT2"), mdSyncsafe(length(stored)), as.raw(c(0, 2)), stored)
  f <- mdFile(mdId3Tag(frame), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "AÿB")
})

test_that("A frame carrying a data length indicator is read past it", {
  text <- c(as.raw(0), charToRaw("Indicated"))
  body <- c(mdSyncsafe(length(text)), text)
  frame <- c(charToRaw("TIT2"), mdSyncsafe(length(body)), as.raw(c(0, 1)), body)
  f <- mdFile(mdId3Tag(frame), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "Indicated")
})

test_that("A compressed frame is passed over rather than read as text", {
  frame <- c(
    charToRaw("TIT2"), mdSyncsafe(8L), as.raw(c(0, 8)), as.raw(c(1:8))
  )
  f <- mdFile(mdId3Tag(frame), ".mp3")
  t <- id3(f)

  expect_length(t, 0)
  expect_identical(attr(t, "skipped")[["TIT2"]], 8)
})

test_that("Text written as UTF-16 with a byte order mark is read", {
  body <- c(as.raw(1), as.raw(c(255, 254)), as.raw(c(0x41, 0, 0x42, 0)))
  f <- mdFile(mdId3Tag(mdFrame24("TIT2", body)), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "AB")
})

test_that("Text written as UTF-16 with no byte order mark is read big-endian", {
  body <- c(as.raw(2), as.raw(c(0, 0x41, 0, 0x42)))
  f <- mdFile(mdId3Tag(mdFrame24("TIT2", body)), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "AB")
})

test_that("Text written as UTF-8 is read", {
  body <- c(as.raw(3), charToRaw("Hirundo"), as.raw(c(0xc2, 0xa9)))
  f <- mdFile(mdId3Tag(mdFrame24("TIT2", body)), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "Hirundo©")
})

test_that("A version 1 tag at the end of the file is read", {
  tag <- c(
    charToRaw("TAG"), mdPad("A Title", 30L), mdPad("An Artist", 30L),
    mdPad("An Album", 30L), mdPad("2019", 4L), mdPad("A comment", 30L), as.raw(12)
  )
  f <- mdFile(c(mdMp3Frame(), tag), ".mp3")
  t <- id3(f)

  expect_identical(attr(t, "version"), "1.0")
  expect_identical(t[["title"]], "A Title")
  expect_identical(t[["artist"]], "An Artist")
  expect_identical(t[["album"]], "An Album")
  expect_identical(t[["year"]], "2019")
  expect_identical(t[["comment"]], "A comment")
  expect_identical(t[["genre"]], 12L)
  expect_null(t[["track"]])
})

# Version 1.1 took two bytes off the comment to make room for a track number,
# and says so by leaving the first of them a nul.
test_that("A version 1.1 tag gives a track number", {
  tag <- c(
    charToRaw("TAG"), mdPad("A Title", 30L), mdPad("An Artist", 30L),
    mdPad("An Album", 30L), mdPad("2019", 4L), mdPad("A comment", 28L),
    as.raw(0), as.raw(7), as.raw(12)
  )
  f <- mdFile(c(mdMp3Frame(), tag), ".mp3")
  t <- id3(f)

  expect_identical(attr(t, "version"), "1.1")
  expect_identical(t[["track"]], 7L)
  expect_identical(t[["comment"]], "A comment")
})

test_that("A genre of 255, which is how a writer records none, is not reported", {
  tag <- c(
    charToRaw("TAG"), mdPad("A Title", 30L), raw(30), raw(30), raw(4), raw(30),
    as.raw(255)
  )
  f <- mdFile(c(mdMp3Frame(), tag), ".mp3")

  expect_null(id3(f)[["genre"]])
})

test_that("A version 2 tag is read in preference to a version 1 tag", {
  tag1 <- c(
    charToRaw("TAG"), mdPad("The older title", 30L), raw(30), raw(30), raw(4),
    raw(30), as.raw(255)
  )
  f <- mdFile(c(mdXenoCantoTag(), mdMp3Frame(), tag1), ".mp3")

  expect_identical(id3(f)[["TIT2"]], "Pacific Swallow (Hirundo tahitica)")
})

# A wave file keeps a whole version 2 tag in a chunk of its own.
test_that("A tag held in a chunk of a wave file is read", {
  f <- mdFile(mdWav(mdFmt(), mdData(), mdChunk("id3 ", mdXenoCantoTag())))

  expect_identical(id3(f)[["TIT2"]], "Pacific Swallow (Hirundo tahitica)")
})

# The three letters that mark a version 1 tag can be spelled by the audio of a
# wave file, which would invent a record the file does not carry.
test_that("The end of a wave file is not searched for a version 1 tag", {
  tag <- c(
    charToRaw("TAG"), mdPad("Not a title", 30L), raw(30), raw(30), raw(4),
    raw(30), as.raw(255)
  )
  f <- mdFile(mdWav(mdFmt(), mdChunk("data", c(raw(16), tag))))

  expect_length(id3(f), 0)
})

# "TAG" and then a run of nuls is what a tagger writes when it has nothing to
# say. The genre byte is then 0, which the format defines as a real genre, so a
# reader that took it at its word would report a record that is not there.
test_that("A version 1 tag holding no text at all is not reported", {
  f <- mdFile(c(mdMp3Frame(), charToRaw("TAG"), raw(125)), ".mp3")

  expect_length(id3(f), 0)
})

test_that("A file with no tag gives an empty result", {
  f <- mdFile(c(mdMp3Frame(), raw(200)), ".mp3")

  expect_s3_class(id3(f), "id3")
  expect_length(id3(f), 0)
})

test_that("A tag of a version this package does not read is refused, not guessed at", {
  f <- mdFile(mdId3Tag(mdTextFrame("TIT2", "From the future"), major=9L), ".mp3")

  expect_warning(t <- id3(f), "this package reads versions 2.2 to 2.4")
  expect_length(t, 0)
})

test_that("A tag whose frames run past the end of it ends the reading", {
  frame <- c(charToRaw("TIT2"), mdSyncsafe(5000L), raw(2), charToRaw("short"))
  f <- mdFile(mdId3Tag(c(mdTextFrame("TPE1", "A Recordist"), frame)), ".mp3")
  t <- id3(f)

  expect_identical(names(t), "TPE1")
})
