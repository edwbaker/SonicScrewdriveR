# The sound libraries this reader was written against put the whole of their
# record for a recording into the description of the chunk.
test_that("The fields of a bext chunk are read as the specification lays them out", {
  record <- "Acinipe calabra; Studio recording; Italy, Sicily, Baucina (PA); Female song"
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(record, "Orthoptera Library"), mdData()))
  b <- bext(f)

  expect_s3_class(b, "bext")
  expect_identical(b[["Description"]], record)
  expect_identical(b[["Originator"]], "Orthoptera Library")
  expect_identical(b[["OriginatorReference"]], "REF123")
  expect_identical(b[["OriginationDate"]], "2011-05-23")
  expect_identical(b[["OriginationTime"]], "14:22:03")
  expect_identical(b[["Version"]], 1L)
  expect_identical(b[["CodingHistory"]], "A=PCM,F=48000,W=24,M=mono")
})

test_that("The date and the time are reported together as one time", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk("A recording"), mdData()))
  t <- bext(f)[["datetime"]]

  expect_s3_class(t, "POSIXct")
  expect_identical(format(t, "%Y-%m-%d %H:%M:%S"), "2011-05-23 14:22:03")
})

# A writer that puts something other than a time in the time field says nothing
# about the date beside it, which the file gave plainly. Leaving the time empty
# already gives the date at midnight, so a bad time should cost no more than a
# missing one rather than taking the date down with it.
test_that("A date is kept when the time beside it cannot be read", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(
    "A recording", date="2011-05-23", time="--:--:--"
  ), mdData()))
  b <- bext(f)

  expect_identical(b[["OriginationDate"]], "2011-05-23")
  expect_s3_class(b[["datetime"]], "POSIXct")
  expect_identical(
    format(b[["datetime"]], "%Y-%m-%d %H:%M:%S"), "2011-05-23 00:00:00"
  )
})

# 16930.wav of the bio.acousti.ca corpus holds "14:00" in a field the
# specification gives eight characters to. The hour and minute are there to be
# read, and reading them is worth more than the midnight a file with no time at
# all gives.
test_that("A time cut short to the minute keeps its hour and minute", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(
    "A recording", date="2011-05-23", time="14:22"
  ), mdData()))

  expect_identical(
    format(bext(f)[["datetime"]], "%Y-%m-%d %H:%M:%S"), "2011-05-23 14:22:00"
  )
})

# Reading the hour and minute of a time stops at the minute and says nothing of
# what follows, so trying it on a whole time would report the right minute and
# quietly lose the seconds. The whole time has to be tried first, and this is
# the test that says so.
test_that("A whole time keeps its seconds", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(
    "A recording", date="2011-05-23", time="14:22:59"
  ), mdData()))

  expect_identical(
    format(bext(f)[["datetime"]], "%Y-%m-%d %H:%M:%S"), "2011-05-23 14:22:59"
  )
})

# An hour or a minute outside the clock is not a time cut short, and gives the
# date at midnight as any other unreadable time does.
test_that("A time cut short to nonsense is still not read", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(
    "A recording", date="2011-05-23", time="99:99"
  ), mdData()))

  expect_identical(
    format(bext(f)[["datetime"]], "%Y-%m-%d %H:%M:%S"), "2011-05-23 00:00:00"
  )
})

# The specification asks for hyphens in the date and colons in the time, but
# allows any punctuation between the parts, and writers differ.
test_that("A date written with other punctuation is read the same way", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk(
    "A recording", date="2011:05:23", time="14-22-03"
  ), mdData()))

  expect_identical(
    format(bext(f)[["datetime"]], "%Y-%m-%d %H:%M:%S"), "2011-05-23 14:22:03"
  )
})

test_that("A chunk with no date gives no time", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk("A recording", date="", time=""), mdData()))
  b <- bext(f)

  expect_identical(b[["Description"]], "A recording")
  expect_null(b[["datetime"]])
})

# Every field of the chunk is written whether or not it holds anything, so a
# reader that reported them all would report a file as carrying a great deal
# that is in fact empty.
test_that("Fields that hold nothing are not reported", {
  f <- mdFile(mdWav(mdFmt(), mdBextChunk("A recording", originator="", history=""), mdData()))
  b <- bext(f)

  expect_false("Originator" %in% names(b))
  expect_false("CodingHistory" %in% names(b))
  expect_false("UMID" %in% names(b))
  expect_false("TimeReference" %in% names(b))
})

test_that("A unique material identifier is reported as the bytes it holds", {
  chunk <- mdBextChunk("A recording")
  # The identifier sits at bytes 349 to 412 of the body, which starts nine
  # bytes into the chunk this builder returns.
  chunk[(8L + 349L):(8L + 352L)] <- as.raw(c(0x06, 0x0a, 0x2b, 0x34))
  f <- mdFile(mdWav(mdFmt(), chunk, mdData()))

  expect_match(bext(f)[["UMID"]], "^060A2B34")
})

test_that("A file with no bext chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(bext(f), "bext")
  expect_length(bext(f), 0)
})

# A recording cut short by a flat battery or a full card is common, and the
# start of the description it had got to is worth more than nothing at all.
# The chunk is the last thing in the file, which is what a file that stopped
# part way through writing one looks like.
test_that("A chunk cut short is read as far as it goes", {
  whole <- mdWav(mdFmt(), mdData(), mdBextChunk("Acinipe calabra; Studio recording"))
  f <- mdFile(whole[seq_len(length(whole) - 568L)])
  b <- bext(f)

  expect_identical(b[["Description"]], "Acinipe calabra; Studio recording")
  expect_null(b[["Originator"]])
  expect_null(b[["CodingHistory"]])
})
