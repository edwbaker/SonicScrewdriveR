# The comment the collection writes, as its files hold it. Built here rather
# than committed so that what each test is about is written where the test is.
xcComment <- function(
  id = 125727,
  recordist = "Richard E. Webster",
  place = "Rose Mine Valley, California (34.2549, -116.7111), United States",
  elevation = "2000m",
  when = "07:00h, 2006-05-05",
  type = "song",
  rest = character(0)
) {
  paste(c(
    paste0("XC", id, " © ", recordist), place, elevation, when, type, rest
  ), collapse = " // ")
}

xc <- function(...) sonicscrewdriver:::.mdXenoCanto(xcComment(...))

test_that("A comment the collection wrote is read into its parts", {
  d <- xc(rest = c(
    "2nd recording today /  / bird-seen:yes /  / playback-used:no",
    "also:  House Finch, Steller's Jay"
  ))
  expect_equal(d$id, 125727L)
  expect_equal(d$recordist, "Richard E. Webster")
  expect_equal(d$locality, "Rose Mine Valley, California")
  expect_equal(d$latitude, 34.2549)
  expect_equal(d$longitude, -116.7111)
  expect_equal(d$country, "United States")
  expect_equal(d$elevation, 2000)
  expect_equal(d$time, "07:00")
  expect_equal(d$date, "2006-05-05")
  expect_equal(d$type, "song")
  expect_equal(d$also, c("House Finch", "Steller's Jay"))
  expect_equal(d$remarks, "2nd recording today")
  expect_true(d$bird_seen)
  expect_false(d$playback_used)
})

test_that("A comment nobody at the collection wrote is left alone", {
  expect_null(sonicscrewdriver:::.mdXenoCanto("Recorded at 21:06:00 14/05/2026"))
  expect_null(sonicscrewdriver:::.mdXenoCanto("A comment // with // slashes"))
  expect_null(sonicscrewdriver:::.mdXenoCanto(""))
  expect_null(sonicscrewdriver:::.mdXenoCanto(NULL))
  # The mark has to be at the head of it: a catalogue number mentioned in the
  # remarks of somebody else's comment is not a record.
  expect_null(sonicscrewdriver:::.mdXenoCanto("Same individual as XC473302 and XC473303"))
})

test_that("The time is read as written, including a 12-hour clock", {
  n <- function(when) {
    sonicscrewdriver:::.mdNormaliseXenoCanto(xc(when = when))$datetime
  }
  expect_equal(format(n("07:00h, 2006-05-05")), "2006-05-05 07:00:00")
  expect_equal(format(n("09:58:59h, 2015-07-30")), "2015-07-30 09:58:59")
  # "2:00 pm" read as a 24-hour clock would be twelve hours early.
  expect_equal(format(n("2:00 pmh, 2012-04-01")), "2012-04-01 14:00:00")
  expect_equal(format(n("12:30 amh, 2012-04-01")), "2012-04-01 00:30:00")
  expect_equal(format(n("06:10:00 AMh, 1991-02-04")), "1991-02-04 06:10:00")
})

test_that("A time the recordist left unsaid gives no datetime, and keeps the date", {
  for (when in c("?h, 1995-03-25", "?:?h, 2003-12-30", "xx:xxh, 2004-01-02",
                 "amh, 2009-04-14", "10;45amh, 2008-09-15")) {
    d <- xc(when = when)
    expect_null(sonicscrewdriver:::.mdNormaliseXenoCanto(d)$datetime)
  }
  # The date is still there to be read.
  expect_equal(xc(when = "?h, 1995-03-25")$date, "1995-03-25")
  expect_null(xc(when = "?h, 1995-03-25")$time)
})

test_that("A date the collection did not have gives no datetime", {
  for (when in c("07:00h, 1974-04-00", "07:00h, 1997-01-00", "07:00h, 0000-00-00")) {
    expect_null(sonicscrewdriver:::.mdNormaliseXenoCanto(xc(when = when))$datetime)
  }
})

test_that("A place without a position still gives its locality and country", {
  d <- xc(place = "Finca Yerbabuenal, canon del rio Toche, Tolima, Colombia")
  expect_null(d$latitude)
  expect_equal(d$country, "Colombia")
  expect_equal(d$locality, "Finca Yerbabuenal, canon del rio Toche, Tolima")
  expect_null(sonicscrewdriver:::.mdNormaliseXenoCanto(d)$location)
})

test_that("A position outside the range of its units is not read as one", {
  expect_null(xc(place = "Nowhere (999.0, 1.0), Country")$latitude)
  expect_null(xc(place = "Nowhere (1.0, 999.0), Country")$longitude)
})

test_that("An elevation that is a range or a placeholder gives no number", {
  expect_equal(xc(elevation = "190m")$elevation, 190)
  expect_equal(xc(elevation = "0.5m")$elevation, 0.5)
  for (e in c("?m", "m", "-m", "2600 - 2800m", "200 - 500m")) {
    expect_null(xc(elevation = e)$elevation)
  }
  # The text is kept whatever it says, so a range is not simply lost.
  expect_equal(xc(elevation = "200 - 500m")$stated_elevation, "200 - 500m")
})

test_that("A comment ending in a separator does not keep it", {
  d <- sonicscrewdriver:::.mdXenoCanto(
    "XC784722 © Tero Linjama // Ao Phang-nga (8.4063, 98.5112), Thailand // 10m // 08:00h, 2023-01-29 //"
  )
  expect_equal(d$date, "2023-01-29")
  expect_equal(format(sonicscrewdriver:::.mdNormaliseXenoCanto(d)$datetime),
               "2023-01-29 08:00:00")
})

test_that("The other species audible are not the species of the recording", {
  d <- xc(rest = "also:  House Finch, Steller's Jay")
  expect_equal(d$also, c("House Finch", "Steller's Jay"))
  expect_null(sonicscrewdriver:::.mdNormaliseXenoCanto(d)$species)
})

test_that("The record reaches readMetadata from either tag dialect", {
  body <- function(text) {
    c(as.raw(3), charToRaw("eng"), as.raw(0), mdUtf8(text))
  }
  comment <- xcComment(rest = "bird-seen:yes /  / playback-used:no")

  # The older files write the comment into COMM.
  f <- mdFile(c(mdId3Tag(mdFrame24("COMM", body(comment))), mdMp3Frame()), ".mp3")
  m <- readMetadata(f, filename = FALSE)
  expect_equal(m$sources$xenoCanto$id, 125727L)
  expect_equal(format(m$fields$datetime), "2006-05-05 07:00:00")
  expect_equal(unname(m$fields$location), c(34.2549, -116.7111))
  expect_equal(m$fields$elevation, 2000)
  expect_equal(m$fields$collection, "xeno-canto")
  # The comment itself is still reported as the tag holds it.
  expect_equal(m$fields$comment, comment)

  # The newer ones write it into a TXXX frame described as "comment".
  txxx <- c(as.raw(3), charToRaw("comment"), as.raw(0), mdUtf8(comment))
  f <- mdFile(c(mdId3Tag(mdFrame24("TXXX", txxx)), mdMp3Frame()), ".mp3")
  m <- readMetadata(f, filename = FALSE)
  expect_equal(m$sources$xenoCanto$id, 125727L)
  expect_equal(format(m$fields$datetime), "2006-05-05 07:00:00")
})

test_that("A file the collection never held has no xenoCanto source", {
  f <- mdFile(c(mdId3Tag(mdTextFrame("TIT2", "A Title")), mdMp3Frame()), ".mp3")
  expect_null(readMetadata(f, filename = FALSE)$sources$xenoCanto)
})

test_that("The catalogue number is the recording's, not the file's", {
  # A recording downloaded from the collection and uploaded elsewhere keeps the
  # comment and gains a new name, so the number need not match the filename.
  body <- c(as.raw(3), charToRaw("eng"), as.raw(0), mdUtf8(xcComment(id = 473302)))
  f <- mdFile(c(mdId3Tag(mdFrame24("COMM", body)), mdMp3Frame()), ".mp3")
  m <- readMetadata(f, filename = FALSE)
  expect_equal(m$sources$xenoCanto$id, 473302L)
})

test_that("A recorder's own record outranks the collection's", {
  # GUANO is the device's own writing; the comment is the collection's
  # transcription of what it was told.
  body <- c(as.raw(3), charToRaw("eng"), as.raw(0), mdUtf8(xcComment()))
  w <- mdWav(
    mdFmt(), mdData(),
    mdGuanoChunk("Timestamp:2020-01-02T03:04:05", "Loc Position:1.5 2.5"),
    mdChunk("id3 ", mdId3Tag(mdFrame24("COMM", body)))
  )
  m <- readMetadata(w)
  expect_equal(format(m$fields$datetime), "2020-01-02 03:04:05")
  expect_equal(unname(m$fields$location), c(1.5, 2.5))
  # and the collection's reading is still there
  expect_equal(m$sources$xenoCanto$latitude, 34.2549)
})

test_that("A catalogue number alone is not a record", {
  # The first bytes of a comment, read off a tag that holds no more than that,
  # carry the number without anything the record is made of. Reading them as a
  # record would put the whole fragment in the recordist.
  expect_null(sonicscrewdriver:::.mdXenoCanto("XC428067 Pacific Swallow \u00a9"))
  expect_null(sonicscrewdriver:::.mdXenoCanto("XC428067"))
  expect_null(sonicscrewdriver:::.mdXenoCanto("XC428067 Pacific Swallow"))
})
