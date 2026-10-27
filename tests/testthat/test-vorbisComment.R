mdComments <- function() {
  c(
    "TITLE=Hirundo tahitica",
    "ARTIST=A Recordist",
    "DATE=2018-02-14",
    "LICENSE=CC BY-NC-SA 4.0",
    "SPECIES=Hirundo tahitica",
    "COMMENT=calling from a wire"
  )
}

test_that("The comments of a FLAC file are read", {
  block <- mdVorbisBlock("reference libFLAC 1.3.4", mdComments())
  f <- mdFile(mdFlac(block), ".flac")
  v <- vorbisComment(f)

  expect_s3_class(v, "vorbisComment")
  expect_identical(v[["vendor"]], "reference libFLAC 1.3.4")
  expect_identical(v[["TITLE"]], "Hirundo tahitica")
  expect_identical(v[["ARTIST"]], "A Recordist")
  expect_identical(v[["LICENSE"]], "CC BY-NC-SA 4.0")
  expect_identical(v[["COMMENT"]], "calling from a wire")
})

# The comments sit behind a block of stream information and, in a real file,
# often behind a seek table and embedded artwork as well, which can be far
# larger than everything else in the file put together.
test_that("The comments are found behind the blocks that come before them", {
  block <- mdVorbisBlock("libFLAC", "TITLE=Behind the artwork")
  f <- mdFile(c(
    charToRaw("fLaC"),
    as.raw(0), mdBE24(34), raw(34),                       # the stream information
    as.raw(3), mdBE24(18), raw(18),                       # a seek table
    as.raw(6), mdBE24(2048), raw(2048),                   # embedded artwork
    as.raw(4), mdBE24(length(block)), block,
    as.raw(128 + 1), mdBE24(4), raw(4)
  ), ".flac")

  expect_identical(vorbisComment(f)[["TITLE"]], "Behind the artwork")
})

test_that("A FLAC file with no comment block gives an empty result", {
  f <- mdFile(c(
    charToRaw("fLaC"),
    as.raw(128), mdBE24(34), raw(34)
  ), ".flac")

  expect_s3_class(vorbisComment(f), "vorbisComment")
  expect_length(vorbisComment(f), 0)
})

test_that("The comments of an Ogg Vorbis file are read", {
  block <- mdVorbisBlock("Xiph.Org libVorbis", mdComments())
  f <- mdFile(c(
    charToRaw("OggS"), raw(23), as.raw(3), charToRaw("vorbis"), block
  ), ".ogg")

  expect_identical(vorbisComment(f)[["ARTIST"]], "A Recordist")
})

test_that("The comments of an Ogg Opus file are read", {
  block <- mdVorbisBlock("libopus 1.4", "TITLE=An Opus recording")
  f <- mdFile(c(
    charToRaw("OggS"), raw(23), charToRaw("OpusTags"), block
  ), ".ogg")

  expect_identical(vorbisComment(f)[["TITLE"]], "An Opus recording")
})

# The format reserves no names, so a name this package has never seen is read
# as readily as one it has.
test_that("A comment of any name is read", {
  block <- mdVorbisBlock("libFLAC", c("BEHAVIOUR=song", "WEATHER=overcast"))
  f <- mdFile(mdFlac(block), ".flac")
  v <- vorbisComment(f)

  expect_identical(v[["BEHAVIOUR"]], "song")
  expect_identical(v[["WEATHER"]], "overcast")
})

test_that("A name used twice keeps both of its values", {
  block <- mdVorbisBlock("libFLAC", c("ARTIST=One", "ARTIST=Two"))
  f <- mdFile(mdFlac(block), ".flac")
  v <- vorbisComment(f)

  expect_identical(v[["ARTIST"]], "One")
  expect_identical(v[["ARTIST.2"]], "Two")
})

test_that("A value holding an equals sign keeps it", {
  block <- mdVorbisBlock("libFLAC", "SETTINGS=gain=2,filter=none")
  f <- mdFile(mdFlac(block), ".flac")

  expect_identical(vorbisComment(f)[["SETTINGS"]], "gain=2,filter=none")
})

test_that("Something that is not a comment at all is passed over", {
  block <- mdVorbisBlock("libFLAC", c("not a comment", "TITLE=A title"))
  f <- mdFile(mdFlac(block), ".flac")
  v <- vorbisComment(f)

  expect_identical(names(v), c("vendor", "TITLE"))
})

test_that("Comments are read as UTF-8, which is what the format requires", {
  block <- mdVorbisBlock("libFLAC", "TITLE=Hirundo © 2018")
  f <- mdFile(mdFlac(block), ".flac")

  expect_identical(vorbisComment(f)[["TITLE"]], "Hirundo © 2018")
})

test_that("A block declaring more comments than it holds is read as far as it goes", {
  block <- c(
    mdU32(7), charToRaw("libFLAC"),
    mdU32(50),                                        # fifty comments claimed
    mdU32(13), charToRaw("TITLE=A title")             # one comment written
  )
  f <- mdFile(mdFlac(block), ".flac")
  v <- vorbisComment(f)

  expect_identical(v[["TITLE"]], "A title")
})

test_that("A wave file is not searched for Vorbis comments", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_length(vorbisComment(f), 0)
})

test_that("The comments of a file held as bytes are read as from the file", {
  bytes <- mdFlac(mdVorbisBlock("libFLAC", mdComments()))
  f <- mdFile(bytes, ".flac")

  expect_identical(unclass(vorbisComment(bytes)), unclass(vorbisComment(f)))
})
