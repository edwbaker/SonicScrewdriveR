# The DISP chunk of a wave file: one clipboard item, introduced by the number
# of the clipboard format it is in. Most writers that mention the chunk leave
# nothing in it but that number, so holding nothing is the common case and is
# tested as carefully as holding a title.

#' A DISP chunk holding one clipboard item of the given format
mdDispChunk <- function(code, body = raw(0)) {
  return(mdChunk("DISP", c(mdU32(code), body)))
}

#' A wave file carrying the given DISP chunks
mdDispWav <- function(...) mdWav(mdFmt(), ..., mdData())

test_that("Text written as the text format is read", {
  f <- mdFile(mdDispWav(mdDispChunk(1, charToRaw("Dawn chorus, Poring"))))
  d <- disp(f)

  expect_s3_class(d, "disp")
  expect_identical(d[["format"]], "CF_TEXT")
  expect_identical(d[["text"]], "Dawn chorus, Poring")
})

test_that("The nul that ends a clipboard string is not part of the string", {
  f <- mdFile(mdDispWav(
    mdDispChunk(1, c(charToRaw("Kinabalu Park"), as.raw(0)))
  ))

  expect_identical(disp(f)[["text"]], "Kinabalu Park")
})

test_that("Text written as the wide text format is read as UTF-16", {
  f <- mdFile(mdDispWav(mdDispChunk(13, mdUtf16("Maïdo, Réunion"))))
  d <- disp(f)

  expect_identical(d[["format"]], "CF_UNICODETEXT")
  expect_identical(d[["text"]], "Maïdo, Réunion")
})

# This is what most of the files that have the chunk at all look like: the
# writer named a format and put nothing in it. There is nothing to report.
test_that("A chunk holding nothing but the format number gives nothing", {
  f <- mdFile(mdDispWav(mdDispChunk(1)))

  expect_s3_class(disp(f), "disp")
  expect_length(disp(f), 0)
})

test_that("A chunk too short to hold a format number gives nothing", {
  f <- mdFile(mdDispWav(mdChunk("DISP", as.raw(c(1, 0)))))

  expect_length(disp(f), 0)
})

# An image says nothing this package can read, but a caller is better told
# that the file carries one than left to think it carries nothing.
test_that("An item in a format that is not text is named and not decoded", {
  f <- mdFile(mdDispWav(mdDispChunk(2, as.raw(c(66, 77, 0, 1)))))
  d <- disp(f)

  expect_identical(d[["format"]], "CF_BITMAP")
  expect_false("text" %in% names(d))
})

test_that("A format number that is not one of the defined ones is given as it stands", {
  f <- mdFile(mdDispWav(mdDispChunk(9999, as.raw(c(1, 2, 3, 4)))))

  expect_identical(disp(f)[["format"]], "9999")
})

test_that("A file holding more than one chunk keeps every one of them", {
  f <- mdFile(mdDispWav(
    mdDispChunk(1, charToRaw("First")),
    mdDispChunk(1, charToRaw("Second"))
  ))
  d <- disp(f)

  expect_identical(d[["text"]], "First")
  expect_identical(d[["text.2"]], "Second")
  expect_identical(d[["format.2"]], "CF_TEXT")
})

test_that("A file with no DISP chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(disp(f), "disp")
  expect_length(disp(f), 0)
})

test_that("The bytes of a file are read as the file is", {
  bytes <- mdDispWav(mdDispChunk(1, charToRaw("Dawn chorus")))

  expect_identical(unclass(disp(bytes)), unclass(disp(mdFile(bytes))))
})

test_that("A disp object given back is the one that was given", {
  d <- disp(mdFile(mdDispWav(mdDispChunk(1, charToRaw("Dawn chorus")))))

  expect_identical(disp(d), d)
})

test_that("A file that is not audio gives an empty result", {
  f <- mdFile(charToRaw("this is not audio, not even slightly"), ".wav")

  expect_length(disp(f), 0)
})

test_that("A file that does not exist says so", {
  expect_error(disp(file.path(tempdir(), "no-such-recording.wav")), "does not exist")
})

test_that("An empty result says which format it found nothing of", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_output(print(disp(f)), "<no display name metadata>")
})

# ------------------------------------------------- the normalised record ---

test_that("A display name is the title where nothing else gives one", {
  m <- readMetadata(
    mdFile(mdDispWav(mdDispChunk(1, charToRaw("Dusk chorus, Poring")))),
    filename = FALSE
  )

  expect_identical(m$fields$title, "Dusk chorus, Poring")
})

# A name holds whatever a program was to show in a list of files, where an
# INFO list holds a field meant for the name of the recording.
test_that("A title written as a field of its own outranks a display name", {
  m <- readMetadata(
    mdFile(mdWav(
      mdFmt(),
      mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Name"))),
      mdDispChunk(1, charToRaw("Dusk chorus, Poring")),
      mdData()
    )),
    filename = FALSE
  )

  expect_identical(m$fields$title, "A Name")
  expect_identical(m$sources$disp[["text"]], "Dusk chorus, Poring")
})

test_that("A display name is no longer counted among the chunks nothing reads", {
  m <- readMetadata(
    mdFile(mdDispWav(mdDispChunk(1, charToRaw("Dusk chorus")))),
    filename = FALSE
  )

  expect_false("DISP" %in% m$unparsed)
  expect_true("disp" %in% names(m$sources))
})

# A chunk that held nothing was still read, so it is not something nothing
# here can read, and the source it would have given is left out.
test_that("A chunk holding nothing leaves no source behind and no complaint", {
  m <- readMetadata(mdFile(mdDispWav(mdDispChunk(1))), filename = FALSE)

  expect_false("DISP" %in% m$unparsed)
  expect_false("disp" %in% names(m$sources))
})
