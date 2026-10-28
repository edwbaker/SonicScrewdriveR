# The helpers the readers share. Most are exercised through the readers
# themselves, but the cases that matter here are the ones a real file reaches
# rarely and a reader cannot be steered into on purpose: bytes that are not
# what their encoding says they are, text in UTF-16, and runs of bytes that
# hold nothing at all.

test_that("A size is read seven bits to the byte, however short it is", {
  # ID3v2.4 writes every size this way, so that no byte of one can be mistaken
  # for the start of an MP3 frame.
  expect_identical(.rawToSyncsafe(as.raw(c(0, 0, 2, 1))), 257)
  expect_identical(.rawToSyncsafe(as.raw(c(0, 0, 0, 127))), 127)
  # The top bit of each byte is not part of the number, and is dropped rather
  # than trusted: a writer that sets it has written a size no reader agrees on.
  expect_identical(.rawToSyncsafe(as.raw(c(0, 0, 0, 255))), 127)
  expect_identical(.rawToSyncsafe(raw(0)), 0)
})

test_that("A big-endian size is read, however short it is", {
  expect_identical(.rawToUIntBE(as.raw(c(1, 0))), 256)
  expect_identical(.rawToUIntBE(as.raw(255)), 255)
  expect_identical(.rawToUIntBE(raw(0)), 0)
})

# A nul is padding in a single-byte encoding and half a character in UTF-16, so
# which of them go depends on what the text is written in.
test_that("Padding is dropped, and UTF-16 keeps the nuls inside it", {
  expect_identical(.mdStripNuls(raw(0)), raw(0))
  expect_identical(.mdStripNuls(as.raw(c(65, 0, 66, 0))), as.raw(c(65, 66)))
  # Two bytes to a character, so the padding goes two bytes at a time and the
  # nul that is half of "A" stays.
  expect_identical(
    .mdStripNuls(as.raw(c(0, 65, 0, 0)), from="UTF-16BE"),
    as.raw(c(0, 65))
  )
  expect_identical(.mdStripNuls(as.raw(0), from="UTF-16BE"), as.raw(0))
})

test_that("A run of bytes holding nothing reads as nothing", {
  expect_identical(.mdDecode(raw(0)), "")
  expect_identical(.mdDecode(raw(4)), "")
  expect_identical(.mdDecodeValues(raw(0)), character(0))
  expect_identical(.mdDecodeValues(raw(3)), character(0))
  expect_identical(.mdSplitNuls(raw(0)), list())
  expect_identical(.mdSplitNuls(as.raw(0), from="UTF-16BE"), list())
})

test_that("One value gives one value, and several give several", {
  expect_identical(.mdDecodeValues(charToRaw("One")), "One")
  expect_identical(
    .mdDecodeValues(c(charToRaw("One"), as.raw(0), charToRaw("Two"))),
    c("One", "Two")
  )
  # A nul at an odd offset in UTF-16 is half an ordinary character rather than
  # a separator, so only a whole nul unit splits the values.
  expect_identical(
    .mdDecodeValues(
      c(as.raw(c(0, 65)), as.raw(c(0, 0)), as.raw(c(0, 66))), from="UTF-16BE"
    ),
    c("A", "B")
  )
})

# Four MP3s in the corpora this was written against hold tags whose bytes are
# not valid in the encoding they declare, which made a naive reader fail with
# "input string 1 is invalid UTF-8" and take every later call down with it.
test_that("Bytes that are not what their encoding says are still read", {
  # A lone high byte is not valid UTF-8, so declaring UTF-8 is a lie here.
  broken <- c(charToRaw("Online"), as.raw(0x92), charToRaw("s"))
  read <- .mdDecode(broken, from="UTF-8")

  expect_type(read, "character")
  expect_length(read, 1L)
  # Whatever happened, what comes back is valid UTF-8: a string marked as UTF-8
  # that is not makes every later call on it fail.
  expect_false(is.na(read))
  expect_true(validUTF8(read))
  expect_match(read, "Online")
  # And it can be used, which is the point of handing back something valid.
  expect_silent(nchar(read))
  expect_silent(gsub("x", "y", read))
})

test_that("An encoding this platform cannot read at all still gives text", {
  read <- .mdDecode(charToRaw("Readable"), from="NOT-AN-ENCODING")

  expect_type(read, "character")
  expect_true(validUTF8(read))
})

# Text declared as ISO-8859-1 that is in fact UTF-8 is what xeno-canto writes,
# and a lone high byte, which is what ISO-8859-1 really gives, is not valid
# UTF-8, so the test does not misfire on it.
test_that("UTF-8 written under the wrong name is read as UTF-8", {
  expect_true(.mdIsUtf8(as.raw(c(0xc2, 0xa9))))
  expect_false(.mdIsUtf8(as.raw(0xa9)))
  # All ASCII reads the same either way, so there is nothing to decide.
  expect_false(.mdIsUtf8(charToRaw("plain")))
  expect_identical(.mdDecode(as.raw(c(0xc2, 0xa9))), "©")
})

test_that("A name is only numbered when it would otherwise repeat", {
  expect_identical(.mdUniqueName(list(), "TIT2"), "TIT2")
  expect_identical(.mdUniqueName(list(TIT2="a"), "TIT2"), "TIT2.2")
  expect_identical(.mdUniqueName(list(TIT2="a", TIT2.2="b"), "TIT2"), "TIT2.3")
})

test_that("A field is written as a line of text whatever it holds", {
  expect_identical(.mdFieldText("plain"), "plain")
  expect_identical(.mdFieldText(c("one", "two")), "one, two")
  expect_identical(.mdFieldText(character(0)), "")
  # Raw bytes are reported by their size: a run of hexadecimal says nothing.
  expect_identical(.mdFieldText(as.raw(c(1, 2, 3))), "<3 bytes>")
  # A named vector keeps its names, which is how a position reads.
  expect_identical(
    .mdFieldText(c(latitude=51.5, longitude=-0.1)),
    "latitude=51.5, longitude=-0.1"
  )
  expect_match(
    .mdFieldText(as.POSIXct("2020-11-03 11:24:30", tz="UTC")),
    "^2020-11-03T11:24:30"
  )
})

test_that("A file is named by its bytes, not by its extension", {
  expect_identical(.mdContainer(c(charToRaw("RIFF"), raw(4), charToRaw("WAVE"))), "WAVE")
  expect_identical(.mdContainer(c(charToRaw("RIFF"), raw(4), charToRaw("AVI "))), "RIFF/AVI ")
  expect_identical(.mdContainer(c(charToRaw("fLaC"), raw(8))), "FLAC")
  expect_identical(.mdContainer(c(charToRaw("OggS"), raw(8))), "Ogg")
  expect_identical(.mdContainer(c(charToRaw("FORM"), raw(4), charToRaw("AIFF"))), "AIFF")
  expect_identical(.mdContainer(c(charToRaw("FORM"), raw(4), charToRaw("AIFC"))), "AIFF/AIFC")
  expect_identical(.mdContainer(c(raw(4), charToRaw("ftypM4A "))), "MP4")
  expect_identical(.mdContainer(c(charToRaw("ID3"), raw(9))), "MP3")
  # An MP3 with no tag starts at a frame header, which is eleven set bits.
  expect_identical(.mdContainer(c(as.raw(c(255, 251, 144, 0)), raw(8))), "MP3")
  expect_true(is.na(.mdContainer(charToRaw("not audio at all"))))
  expect_true(is.na(.mdContainer(raw(4))))
})

test_that("A wave file over 4 GiB is named by the form it was written in", {
  rf64 <- c(charToRaw("RF64"), mdU32(4294967295), charToRaw("WAVE"))
  bw64 <- c(charToRaw("BW64"), mdU32(4294967295), charToRaw("WAVE"))
  expect_identical(.mdContainer(rf64), "RF64")
  expect_identical(.mdContainer(bw64), "BW64")

  # Each holds its metadata in wave chunks, which nothing else here does.
  expect_true(.mdWaveContainer("WAVE"))
  expect_true(.mdWaveContainer("RF64"))
  expect_true(.mdWaveContainer("BW64"))
  expect_false(.mdWaveContainer("MP3"))
  expect_false(.mdWaveContainer(NA_character_))

  # Named, a BW64 file is no longer taken for a file of no known form, whose
  # last 128 bytes could be read as a version 1 ID3 tag.
  expect_false(.id3v1Possible(.mdContainer(bw64)))
})

test_that("Bytes are read from a file by position, and past its end", {
  f <- mdFile(charToRaw("0123456789"))

  expect_identical(.mdFileRange(f, 1L, 4L), charToRaw("0123"))
  expect_identical(.mdFileRange(f, 7L, 4L), charToRaw("6789"))
  # However many of the bytes asked for are there is what comes back.
  expect_identical(.mdFileRange(f, 9L, 10L), charToRaw("89"))
  expect_identical(.mdFileRange(f, 1L, 0L), raw(0))
})
