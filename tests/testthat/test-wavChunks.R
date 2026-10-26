# The chunk structure of a file can be read either from its bytes or over a
# connection. Reading the audio of a long recording to find a chunk of a few
# hundred bytes is what the connection avoids, so these tests check that the
# two ways agree, and that walking a file really does leave the audio alone.

flatWaveFile <- function(samples=1000) {
  f <- tempfile(fileext=".WAV")
  w <- tuneR::Wave(left=as.integer(rep(500L, samples)), samp.rate=8000, bit=16)
  tuneR::writeWave(w, f)
  return(f)
}

test_that("The chunks of a file are the same read over a connection", {
  f <- flatWaveFile()
  on.exit(unlink(f))
  guanoWrite(guanoCreate(Make="Open Acoustic Devices"), f)

  expect_identical(
    .wavChunksFile(f),
    .wavChunks(readBin(f, "raw", n=file.size(f)))
  )
})

test_that("The chunks of a recording cut short are the same either way", {
  f <- flatWaveFile()
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
  f <- flatWaveFile(200000)
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
  f <- flatWaveFile()
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

# Reading a file in parts, as expanding a triggered recording does, gives the
# walk the start of a file and the size of the whole of it. A chunk can then sit
# inside the file but outside the bytes in hand, and reading past the end of a
# raw vector gives zero bytes rather than failing, so the fields would be read
# as zeros: a mono 16-bit PCM recording refused for being none of those.
headerPast <- function(fill, fmtSize=16L, fmtBody=NULL, tail=100L) {
  if (is.null(fmtBody)) {
    fmtBody <- c(
      .uintToRaw(1L, 2L), .uintToRaw(1L, 2L), .uintToRaw(8000L, 4L),
      .uintToRaw(16000L, 4L), .uintToRaw(2L, 2L), .uintToRaw(16L, 2L)
    )
  }
  # The data chunk comes first, so that the walk has found it by the time it
  # stops at a format chunk it cannot reach the end of.
  return(c(
    charToRaw("RIFF"), .uintToRaw(0L, 4L), charToRaw("WAVE"),
    charToRaw("data"), .uintToRaw(16L, 4L), raw(16L),
    charToRaw("LIST"), .uintToRaw(fill, 4L), rep(charToRaw("x"), fill),
    charToRaw("fmt "), .uintToRaw(fmtSize, 4L), fmtBody,
    raw(tail)
  ))
}

test_that("A format chunk outside the bytes read is reported, not read as zeros", {
  bytes <- headerPast(964L)
  blocks <- bytes[seq_len((length(bytes) %/% 512L) * 512L)]

  # The chunk is found, and the file says it is not cut short: only the bytes
  # in hand stop short of it.
  fmt <- .wavFindChunk(.wavChunks(blocks, total=length(bytes))$chunks, "fmt ")
  expect_false(fmt$truncated)
  expect_gt(fmt$start + 15L, length(blocks))

  expect_error(
    .wavFormat(blocks, total=length(bytes)),
    class="unsupportedWav"
  )
  # The same bytes, all of them, read the format they really hold.
  expect_equal(.wavFormat(bytes)$bits, 16)
  expect_equal(.wavFormat(bytes)$format, 1)
})

test_that("An extensible sub-format outside the bytes read is reported", {
  # WAVE_FORMAT_EXTENSIBLE keeps the format it extends 24 bytes into the chunk,
  # so a chunk whose first sixteen bytes are in hand can still be short of it.
  extensible <- c(
    .uintToRaw(65534L, 2L), .uintToRaw(1L, 2L), .uintToRaw(8000L, 4L),
    .uintToRaw(16000L, 4L), .uintToRaw(2L, 2L), .uintToRaw(16L, 2L),
    .uintToRaw(22L, 2L), .uintToRaw(16L, 2L), .uintToRaw(4L, 4L),
    .uintToRaw(1L, 2L), raw(14L)
  )
  bytes <- headerPast(948L, fmtSize=40L, fmtBody=extensible, tail=150L)
  blocks <- bytes[seq_len((length(bytes) %/% 512L) * 512L)]

  fmt <- .wavFindChunk(.wavChunks(blocks, total=length(bytes))$chunks, "fmt ")
  expect_lte(fmt$start + 15L, length(blocks))
  expect_gt(fmt$start + 25L, length(blocks))

  expect_error(.wavFormat(blocks, total=length(bytes)), class="unsupportedWav")
  expect_equal(.wavFormat(bytes)$format, 1)
})
