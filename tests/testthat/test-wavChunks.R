# The chunk structure of a file can be read either from its bytes or over a
# connection. Reading the audio of a long recording to find a chunk of a few
# hundred bytes is what the connection avoids, so these tests check that the
# two ways agree, and that walking a file really does leave the audio alone.

sineFile <- function(samples=1000) {
  f <- tempfile(fileext=".WAV")
  w <- tuneR::Wave(left=as.integer(rep(500L, samples)), samp.rate=8000, bit=16)
  tuneR::writeWave(w, f)
  return(f)
}

test_that("The chunks of a file are the same read over a connection", {
  f <- sineFile()
  on.exit(unlink(f))
  guanoWrite(guanoCreate(Make="Open Acoustic Devices"), f)

  expect_identical(
    .wavChunksFile(f),
    .wavChunks(readBin(f, "raw", n=file.size(f)))
  )
})

test_that("The chunks of a recording cut short are the same either way", {
  f <- sineFile()
  on.exit(unlink(f))

  # Cut the audio short without correcting the size the file declares, as a
  # flat battery or a full card would.
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) - 500)], con)
  close(con)

  info <- .wavChunksFile(f)
  expect_equal(info, .wavChunks(readBin(f, "raw", n=file.size(f))))
  expect_true(.wavFindChunk(info$chunks, "data")$truncated)
})

test_that("Walking a file reads its chunk headers and not its audio", {
  f <- sineFile(200000)
  on.exit(unlink(f))
  guanoWrite(guanoCreate(Make="Open Acoustic Devices"), f)

  size <- file.size(f)
  bytesRead <- 0
  con <- file(f, "rb")
  on.exit(close(con), add=TRUE)
  counting <- function(at, n) {
    bytesRead <<- bytesRead + n
    seek(con, at - 1)
    return(readBin(con, "raw", n=n))
  }

  info <- .wavWalk(counting, size, size)
  expect_false(is.null(.wavFindChunk(info$chunks, "guan")))
  expect_gt(size, 100000)
  # Twelve bytes of RIFF header and eight for each chunk header.
  expect_lt(bytesRead, 200)
})

test_that("One chunk can be read from a file without reading the rest", {
  f <- sineFile()
  on.exit(unlink(f))

  expect_null(.wavChunkFromFile(f, "guan"))

  guanoWrite(guanoCreate(Make="Open Acoustic Devices"), f)
  body <- .wavChunkFromFile(f, "guan")
  expect_true(grepl("Make:Open Acoustic Devices", rawToChar(body), fixed=TRUE))

  chunk <- .wavFindChunk(.wavChunksFile(f)$chunks, "guan")
  expect_identical(length(body), as.integer(chunk$size))
  expect_null(.wavChunkFromFile(f, "LIST"))
})

test_that("A file that is not a wave file is reported as unsupported", {
  f <- tempfile(fileext=".WAV")
  on.exit(unlink(f))
  writeLines("not a wave file at all", f)

  expect_error(.wavChunksFile(f), class="unsupportedWav")
  expect_error(.wavChunkFromFile(f, "guan"), class="unsupportedWav")

  # A file too short to hold even a RIFF header.
  writeBin(charToRaw("RIFF"), f)
  expect_error(.wavChunksFile(f), class="unsupportedWav")
})
