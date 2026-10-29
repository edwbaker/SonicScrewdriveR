test_that("A position and the label that names it are read together", {
  f <- mdFile(mdWav(
    mdFmt(rate=48000),
    mdCueChunk(c(0, 96000, 144000)),
    mdAdtl(mdCueField("labl", 1L, "Phrase 1"), mdCueField("labl", 2L, "Call"),
           mdCueField("note", 1L, "A note on the phrase")),
    mdData()
  ))
  got <- wavCues(f, output="data.frame")
  expect_identical(nrow(got), 3L)
  expect_identical(got$id, c(1, 2, 3))
  expect_identical(got$position, c(0, 96000, 144000))
  expect_identical(got$label, c("Phrase 1", "Call", NA))
  expect_identical(got$note, c("A note on the phrase", NA, NA))
})

test_that("The position is given in seconds by the rate the file declares", {
  f <- mdFile(mdWav(mdFmt(rate=48000), mdCueChunk(c(0, 24000, 96000)), mdData()))
  expect_identical(wavCues(f, output="data.frame")$seconds, c(0, 0.5, 2))
  slow <- mdFile(mdWav(mdFmt(rate=8000), mdCueChunk(c(4000)), mdData()))
  expect_identical(wavCues(slow, output="data.frame")$seconds, 0.5)
})

test_that("A file with no format chunk gives positions but no seconds", {
  # Reading the chunks of a file does not depend on its being playable, and a
  # marked position is still a marked position without a rate to divide by.
  f <- mdFile(c(charToRaw("RIFF"), mdU32(100), charToRaw("WAVE"),
                mdCueChunk(c(48000))))
  got <- wavCues(f, output="data.frame")
  expect_identical(got$position, 48000)
  expect_identical(got$seconds, NA_real_)
})

test_that("A labelled region carries its length and what it is for", {
  f <- mdFile(mdWav(
    mdFmt(rate=48000),
    mdCueChunk(c(48000, 96000)),
    mdAdtl(mdLtxtField(1L, 240000, "rgn ", "A dawn chorus"),
           mdCueField("labl", 1L, "Chorus")),
    mdData()
  ))
  got <- wavCues(f, output="data.frame")
  expect_identical(got$length, c(240000, NA))
  # Four characters, as a chunk identifier is: "rgn" is written "rgn ".
  expect_identical(got$purpose, c("rgn ", NA))
  expect_identical(got$text, c("A dawn chorus", NA))
  expect_identical(got$label, c("Chorus", NA))
})

test_that("A cue point with no label is still a position the file marks", {
  # Nine files in the cached corpus declare ninety-nine points and name none
  # of them, which is a writer reserving the room rather than marking anything.
  f <- mdFile(mdWav(mdFmt(), mdCueChunk(rep(0, 20), ids=100:119), mdData()))
  got <- wavCues(f, output="data.frame")
  expect_identical(nrow(got), 20L)
  expect_true(all(is.na(got$label)))
  expect_identical(unique(got$position), 0)
  expect_identical(got$id[1], 100)
})

test_that("A label whose cue point is missing is reported without a position", {
  # The text is metadata the file carries. Dropping it because the chunk that
  # places it has been lost would report less than the file holds.
  f <- mdFile(mdWav(
    mdFmt(rate=48000),
    mdCueChunk(c(48000)),
    mdAdtl(mdCueField("labl", 1L, "Named"), mdCueField("labl", 7L, "Orphaned")),
    mdData()
  ))
  got <- wavCues(f, output="data.frame")
  expect_identical(nrow(got), 2L)
  expect_identical(got$label, c("Named", "Orphaned"))
  expect_identical(got$position, c(48000, NA))
  expect_identical(got$seconds, c(1, NA))
  expect_identical(got$chunk, c("data", NA))
})

test_that("A file holding neither cue points nor labels gives no rows", {
  got <- wavCues(mdFile(mdWav(mdFmt(), mdData())), output="data.frame")
  expect_identical(nrow(got), 0L)
  expect_identical(names(got), c("id", "position", "seconds", "label", "note",
                                 "length", "purpose", "text", "chunk"))
})

test_that("The cues of a file and of its bytes are the same cues", {
  f <- mdFile(mdWav(
    mdFmt(), mdCueChunk(c(0, 4800)),
    mdAdtl(mdCueField("labl", 1L, "A Label")), mdData()
  ))
  expect_identical(wavCues(readBin(f, "raw", n=file.size(f)), output="data.frame"),
                   wavCues(f, output="data.frame"))
})

test_that("A cue chunk declaring more points than it holds is read as far as it goes", {
  # A file cut short declares what the writer meant to write. Reading the room
  # it actually has is the same thing wavChunks() does with a truncated chunk.
  f <- mdFile(mdWav(
    mdFmt(), mdCueChunk(c(0, 4800), declared=99), mdData()
  ))
  got <- wavCues(f, output="data.frame")
  expect_identical(nrow(got), 2L)
  expect_identical(got$position, c(0, 4800))
})

test_that("Labels in more than one list are all read", {
  f <- mdFile(mdWav(
    mdFmt(), mdCueChunk(c(0, 4800)),
    mdAdtl(mdCueField("labl", 1L, "First list")),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Title"))),
    mdAdtl(mdCueField("labl", 2L, "Second list")),
    mdData()
  ))
  expect_identical(wavCues(f, output="data.frame")$label, c("First list", "Second list"))
})

test_that("A point named twice keeps the first name the file gives it", {
  f <- mdFile(mdWav(
    mdFmt(), mdCueChunk(c(0)),
    mdAdtl(mdCueField("labl", 1L, "First"), mdCueField("labl", 1L, "Second")),
    mdData()
  ))
  expect_identical(wavCues(f, output="data.frame")$label, "First")
})

test_that("The chunk a position is into is reported, so it cannot be misread", {
  # A cue into a wave list is an offset into that part of the audio rather than
  # into the recording, and reporting it as though it were the latter would be
  # wrong rather than merely incomplete.
  f <- mdFile(mdWav(mdFmt(), mdCueChunk(c(2400), chunk="slnt"), mdData()))
  expect_identical(wavCues(f, output="data.frame")$chunk, "slnt")
})

test_that("A list field that runs past the end of its list stops the reading", {
  # A list cut short declares a field longer than what is left of it, and a
  # field shorter than four bytes cannot even name the cue point it belongs
  # to. Reading on from either would read whatever follows as a label.
  kept <- mdCueField("labl", 1L, "Kept")
  overrun <- c(charToRaw("labl"), mdU32(400), charToRaw("Lost"))
  f <- mdFile(mdWav(mdFmt(), mdCueChunk(c(0, 4800)),
                    mdAdtl(kept, overrun), mdData()))
  expect_identical(wavCues(f, output="data.frame")$label, c("Kept", NA))

  tooShort <- c(charToRaw("labl"), mdU32(2), charToRaw("ab"))
  g <- mdFile(mdWav(mdFmt(), mdCueChunk(c(0, 4800)),
                    mdAdtl(kept, tooShort), mdData()))
  expect_identical(wavCues(g, output="data.frame")$label, c("Kept", NA))
})

test_that("A file that is not a wave file is refused, as listing its chunks is", {
  expect_error(wavCues(mdFile(charToRaw("not a wave file"))),
               class="unsupportedWav")
  expect_error(wavCues(file.path(tempdir(), "no-such-file.wav")),
               "File does not exist")
})

test_that("An empty cue chunk is a file marking nothing", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("cue ", mdU32(0)), mdData()))
  expect_identical(nrow(wavCues(f, output="data.frame")), 0L)
})

test_that("wavInfo() still reads the text of the same labels", {
  # The labels were readable before this; what was missing was where they are.
  # Both readings stand, so neither has to be chosen over the other.
  f <- mdFile(mdWav(
    mdFmt(), mdCueChunk(c(0)),
    mdAdtl(mdCueField("labl", 1L, "A Label")), mdData()
  ))
  expect_identical(unname(unlist(unclass(wavInfo(f)))), "A Label")
  expect_identical(wavCues(f, output="data.frame")$label, "A Label")
})

test_that("Cue points are given as annotations by default", {
  f <- mdFile(mdWav(
    mdFmt(rate=48000),
    mdCueChunk(c(48000, 96000, 144000)),
    mdAdtl(mdLtxtField(1L, 240000, "rgn ", "A dawn chorus"),
           mdCueField("labl", 1L, "Chorus"),
           mdCueField("note", 2L, "A note"),
           mdLtxtField(3L, 24000, "rgn ", "Region text")),
    mdData()
  ))
  got <- wavCues(f)
  expect_length(got, 3)
  expect_true(all(vapply(got, is, logical(1), "Annotation")))

  # A labelled region runs from its position for its length.
  expect_identical(c(got[[1]]@start, got[[1]]@end), c(1, 6))
  expect_identical(got[[1]]@type, "region")
  expect_identical(got[[1]]@value, "Chorus")
  expect_identical(got[[1]]@source, "wavCues")
  expect_identical(got[[1]]@file, f)
  expect_identical(c(got[[1]]@low, got[[1]]@high), c(0, Inf))
  expect_identical(got[[1]]@metadata$text, "A dawn chorus")
  expect_identical(got[[1]]@metadata$purpose, "rgn ")
  expect_identical(got[[1]]@metadata$id, 1)

  # Any other point starts and ends where it is.
  expect_identical(c(got[[2]]@start, got[[2]]@end), c(2, 2))
  expect_identical(got[[2]]@type, "cue")
  expect_identical(got[[2]]@value, "")
  expect_identical(got[[2]]@metadata$note, "A note")

  # A region with no label is named by its text.
  expect_identical(got[[3]]@value, "Region text")
  expect_identical(c(got[[3]]@start, got[[3]]@end), c(3, 3.5))

  # The metadata are the fields of the data frame, so the two agree.
  frame <- wavCues(f, output="data.frame")
  fields <- c("id", "position", "label", "note", "length", "purpose", "text", "chunk")
  for (i in seq_along(got)) {
    expect_identical(got[[i]]@metadata, as.list(frame[i, fields]))
    expect_identical(got[[i]]@start, frame$seconds[i])
  }
})

test_that("An annotation whose position is not known starts and ends at NA", {
  f <- mdFile(mdWav(
    mdFmt(rate=48000),
    mdCueChunk(c(48000)),
    mdAdtl(mdCueField("labl", 1L, "Named"), mdCueField("labl", 7L, "Orphaned")),
    mdData()
  ))
  got <- wavCues(f)
  expect_identical(got[[2]]@value, "Orphaned")
  expect_identical(c(got[[2]]@start, got[[2]]@end), c(NA_real_, NA_real_))

  # Without a rate there are positions but no seconds.
  g <- mdFile(c(charToRaw("RIFF"), mdU32(100), charToRaw("WAVE"),
                mdCueChunk(c(48000))))
  got <- wavCues(g)
  expect_identical(got[[1]]@start, NA_real_)
  expect_identical(got[[1]]@metadata$position, 48000)
})

test_that("The annotations of bytes name no file, and of nothing are none", {
  f <- mdFile(mdWav(mdFmt(), mdCueChunk(c(0)), mdData()))
  expect_identical(wavCues(readBin(f, "raw", n=file.size(f)))[[1]]@file, "")
  expect_identical(wavCues(mdFile(mdWav(mdFmt(), mdData()))), list())
})

test_that("An unknown output is refused", {
  expect_error(wavCues("marked.wav", output="table"), "Unknown output format.")
})
