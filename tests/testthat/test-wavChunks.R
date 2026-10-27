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

# A recorder that loses power part-way through a file leaves the size of the
# audio unwritten. Telling that from a file that really holds no audio is what
# these tests are about: the first still holds its recording, the second does
# not, and both declare an empty data chunk.
unfinalise <- function(file) {
  bytes <- readBin(file, "raw", n=file.size(file))
  info <- .wavFormat(bytes)
  bytes[(info$dataStart - 4):(info$dataStart - 1)] <- raw(4)
  bytes[5:8] <- .uintToRaw(8, 4L)
  writeBin(bytes, file)
  invisible(file)
}

test_that("A data chunk whose size was never written runs to the end of the file", {
  f <- flatWaveFile(1000)
  on.exit(unlink(f))
  unfinalise(f)

  info <- .wavFormat(readBin(f, "raw", n=file.size(f)))
  expect_true(info$dataUnfinalised)
  expect_equal(info$dataSize, 2000)
  expect_false(info$dataTruncated)

  # The size is the same read over a connection, where the bytes after the
  # chunk header are fetched rather than held.
  chunks <- .wavChunksFile(f)
  data <- .wavFindChunk(chunks$chunks, "data")
  expect_equal(
    .wavUnfinalisedSize(data, chunks$riffSize, 2, file.size(f), NULL),
    2000
  )
})

test_that("A file that holds no audio keeps its empty data chunk", {
  f <- flatWaveFile(1000)
  on.exit(unlink(f))

  # An empty recording with its metadata after it, as an AudioMoth writes when
  # its battery fails as it starts to record: the file accounts for every byte
  # it holds, so there is no audio hiding in it.
  bytes <- readBin(f, "raw", n=file.size(f))
  info <- .wavFormat(bytes)
  head <- bytes[seq_len(info$dataStart - 1)]
  head[(info$dataStart - 4):(info$dataStart - 1)] <- raw(4)
  text <- charToRaw("GUANO|Version:1.0
Length:0")
  guan <- c(charToRaw("guan"), .uintToRaw(length(text), 4L), text)
  head[5:8] <- .uintToRaw(length(head) + length(guan) - 8, 4L)
  writeBin(c(head, guan), f)

  after <- .wavFormat(readBin(f, "raw", n=file.size(f)))
  expect_false(after$dataUnfinalised)
  expect_equal(after$dataSize, 0)
  expect_equal(guano(f)[["Length"]], 0)
})

test_that("A chunk header is told from audio the writer never recorded", {
  header <- c(charToRaw("guan"), .uintToRaw(10, 4L))
  expect_true(.wavIsChunkHeader(header, 18))
  # A chunk that could not fit in what is left of the file is not one.
  expect_false(.wavIsChunkHeader(header, 17))
  # Audio rarely begins with four printable characters.
  expect_false(.wavIsChunkHeader(c(as.raw(c(0, 255, 7, 200)), .uintToRaw(10, 4L)), 1000))
  # Nothing can be told from fewer than eight bytes.
  expect_false(.wavIsChunkHeader(header[1:4], 1000))
})

test_that("Only an empty data chunk with unaccounted bytes after it is unfinalised", {
  chunk <- list(id="data", start=45, size=0, truncated=FALSE)
  # 1044 bytes of audio the RIFF header does not account for.
  expect_equal(.wavUnfinalisedSize(chunk, 8, 2, 1088), 1044)
  # A chunk that declares a size of its own is taken at its word.
  expect_equal(.wavUnfinalisedSize(list(id="data", start=45, size=100, truncated=FALSE), 8, 2, 1088), 0)
  # A RIFF size that accounts for the whole file leaves nothing unrecorded.
  expect_equal(.wavUnfinalisedSize(chunk, 1080, 2, 1088), 0)
  # A block alignment of zero, from a header that gives no channels or bit
  # depth, cannot say how much audio there is.
  expect_equal(.wavUnfinalisedSize(chunk, 8, 0, 1088), 0)
  # Less than one sample frame is not a recording.
  expect_equal(.wavUnfinalisedSize(chunk, 8, 2, 45), 0)
  # The size is rounded down to a whole sample frame.
  expect_equal(.wavUnfinalisedSize(chunk, 8, 4, 1090), 1044)
})

test_that("A chunk written after an empty data chunk is not read as audio", {
  # The writer left the data chunk empty and wrote its metadata after it without
  # counting it in the RIFF header. The bytes after the chunk begin a chunk of
  # their own, so they are not a recording.
  data <- list(id="data", start=45, size=0, truncated=FALSE)
  header <- c(charToRaw("guan"), .uintToRaw(20, 4L))
  expect_equal(
    .wavUnfinalisedSize(data, 8, 2, 1088, function(at, n) header),
    0
  )

  # Audio in the same place is read as the recording it is.
  audio <- c(as.raw(c(0, 255, 7, 200)), .uintToRaw(10, 4L))
  expect_equal(
    .wavUnfinalisedSize(data, 8, 2, 1088, function(at, n) audio),
    1044
  )
})

test_that("The size of an unfinalised chunk is judged without the bytes after it", {
  f <- flatWaveFile(1000)
  on.exit(unlink(f))
  unfinalise(f)

  # Only the start of the file, stopping before the audio, as audiomothExpand()
  # reads it. The bytes after the data chunk header cannot be weighed, so the
  # size the RIFF header declares answers on its own.
  size <- file.size(f)
  bytes <- readBin(f, "raw", n=size)
  info <- .wavFormat(bytes[seq_len(.wavFormat(bytes)$dataStart - 1)], total=size)
  expect_true(info$dataUnfinalised)
  expect_equal(info$dataSize, 2000)
})

test_that("Bytes are copied in bounded buffers", {
  source <- tempfile(fileext=".bin")
  out <- tempfile(fileext=".bin")
  on.exit(unlink(c(source, out)))
  bytes <- as.raw(sample(0:255, 2.5 * 2^20, replace=TRUE))
  writeBin(bytes, source)

  con <- file(source, "rb")
  on.exit(close(con), add=TRUE)
  read <- .wavConReader(con)
  asked <- numeric(0)
  counting <- function(at, n) {
    asked <<- c(asked, n)
    return(read(at, n))
  }

  written <- file(out, "wb")
  .copyBytes(counting, written, 1, length(bytes))
  # A run that ends before it begins is a run of no bytes, and writes none.
  .copyBytes(counting, written, 5, 4)
  close(written)

  expect_identical(readBin(out, "raw", n=file.size(out)), bytes)

  # However long the run, nothing bigger than one buffer is ever asked for.
  expect_gt(length(asked), 2)
  expect_lte(max(asked), 2^20)
})

test_that("Bytes are replaced as they are copied", {
  bytes <- charToRaw("ABCDEFGHIJ")
  read <- function(at, n) bytes[at:(at + n - 1)]
  copy <- function(from, to, edits=list()) {
    con <- rawConnection(raw(0), "wb")
    on.exit(close(con))
    .copyEdited(read, con, from, to, edits)
    return(rawToChar(rawConnectionValue(con)))
  }

  expect_equal(copy(1, 10), "ABCDEFGHIJ")

  # Given in any order, each is written where it belongs.
  expect_equal(
    copy(1, 10, list(list(at=8, bytes=charToRaw("xy")),
                     list(at=2, bytes=charToRaw("..")))),
    "A..DEFGxyJ"
  )

  # An edit outside the run is left for whichever run holds it, which is how
  # the same edits can be given for the bytes before the metadata and after.
  expect_equal(copy(1, 4, list(list(at=8, bytes=charToRaw("xy")))), "ABCD")
  expect_equal(copy(5, 10, list(list(at=2, bytes=charToRaw("..")))), "EFGHIJ")

  # One lying across an end of the run is written as far as the run goes.
  expect_equal(copy(1, 5, list(list(at=4, bytes=charToRaw("wxyz")))), "ABCwx")
  expect_equal(copy(3, 6, list(list(at=1, bytes=charToRaw("wxyz")))), "yzEF")

  # Bytes already written are past answering for, so where two edits cover the
  # same byte the earlier of them writes it, rather than the copy running
  # backwards over what it has already put out.
  expect_equal(
    copy(1, 10, list(list(at=3, bytes=charToRaw("wxyz")),
                     list(at=5, bytes=charToRaw("..")))),
    "ABwxyzGHIJ"
  )
  # Including where the first of them ran to the end of the run, which taken
  # literally would have the second write backwards over it.
  expect_equal(
    copy(1, 4, list(list(at=3, bytes=charToRaw("wxyz")),
                    list(at=4, bytes=charToRaw("pq")))),
    "ABwx"
  )
})
