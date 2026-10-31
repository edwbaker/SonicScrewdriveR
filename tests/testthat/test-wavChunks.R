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

# A recording too long for the four bytes a RIFF header gives a size is written
# in the RF64 or BW64 form, which keeps its sizes in a ds64 chunk and leaves
# 0xFFFFFFFF where a RIFF file would give them. Walking such a file by the
# headers alone would take its audio to run to the end of the file, and miss
# every chunk written after it.

chunkIds <- function(info) vapply(info$chunks, function(chunk) chunk$id, character(1))

test_that("An RF64 file is walked by the sizes its ds64 chunk gives", {
  bytes <- mdRf64Wav()
  f <- mdFile(bytes)

  info <- .wavChunksFile(f)
  expect_identical(info, .wavChunks(bytes))
  expect_identical(info$form, "RF64")
  expect_identical(chunkIds(info), c("ds64", "fmt ", "data", "guan"))
  expect_equal(info$riffSize + 8, length(bytes))

  data <- .wavFindChunk(info$chunks, "data")
  expect_equal(data$size, 200)
  expect_false(data$truncated)
  expect_equal(info$ds64$dataSize, 200)
  expect_equal(info$ds64$sampleCount, 100)
  expect_equal(info$ds64$frameSize, 2)
  expect_true(info$ds64$countAgrees)
})

test_that("A BW64 file is walked as an RF64 file is", {
  info <- .wavChunks(mdRf64Wav(form="BW64"))
  expect_identical(info$form, "BW64")
  expect_identical(chunkIds(info), c("ds64", "fmt ", "data", "guan"))
})

test_that("A RIFF file has no ds64 chunk to go by", {
  info <- .wavChunks(mdWav(mdFmt(), mdData()))
  expect_identical(info$form, "RIFF")
  expect_null(info$ds64)
})

test_that("A ds64 chunk too short to hold a table is read as having none", {
  # FFmpeg reads a ds64 chunk of the three sizes alone, without the count of
  # table entries that would follow them.
  body <- c(mdFmt(), mdDeferred("data", raw(200)))
  ds64 <- c(
    charToRaw("ds64"), mdU32(24),
    mdU64(4 + 32 + length(body)), mdU64(200), mdU64(100)
  )
  bytes <- c(charToRaw("RF64"), mdU32(4294967295), charToRaw("WAVE"), ds64, body)

  info <- .wavChunks(bytes)
  expect_equal(.wavFindChunk(info$chunks, "data")$size, 200)
  expect_length(info$ds64$table, 0)
})

test_that("A chunk too large for its own header is walked past by the table", {
  guan <- mdChunk("guan", charToRaw("GUANO|Version:1.0\n"))
  bytes <- mdRf64Wav(after=c(mdDeferred("JUNK", raw(10)), guan), table=list(JUNK=10))

  info <- .wavChunks(bytes)
  expect_equal(info$ds64$table, c(JUNK=10))
  expect_equal(.wavFindChunk(info$chunks, "JUNK")$size, 10)
  expect_identical(chunkIds(info), c("ds64", "fmt ", "data", "JUNK", "guan"))
})

test_that("A table too long to read whole is read as far as it goes", {
  # 6000 entries is more than is read of the chunk, so the later ones are not
  # looked at, and a chunk that would need one is taken at its own word.
  table <- as.list(rep(10, 6000))
  names(table) <- sprintf("%04d", seq_len(6000))
  info <- .wavChunks(mdRf64Wav(table=table))
  expect_lt(length(info$ds64$table), 6000)
  expect_equal(length(info$ds64$table), (65536 - 28) %/% 12)
  expect_identical(names(info$ds64$table)[1:2], c("0001", "0002"))
})

test_that("A ds64 chunk that cannot be believed is refused", {
  refused <- function(bytes, message) {
    expect_error(.wavChunks(bytes), message, class="badDs64")
    # As every other refusal is, so that what passes over a file it cannot read
    # passes over this one too.
    expect_error(.wavChunks(bytes), class="unsupportedWav")
  }
  header <- c(charToRaw("RF64"), mdU32(4294967295), charToRaw("WAVE"))
  sizes <- c(mdU64(1000), mdU64(200), mdU64(100), mdU32(0))
  audio <- mdDeferred("data", raw(200))

  # The ds64 chunk is missing, comes later than it must, or is too short to be
  # one, and an RF64 file too short to hold one at all.
  refused(c(header, mdFmt(), mdChunk("data", raw(200))), "first chunk is not ds64")
  refused(c(header, mdFmt(), mdChunk("ds64", sizes), audio), "first chunk is not ds64")
  refused(c(header, mdChunk("ds64", raw(16)), mdFmt(), audio), "too short")
  refused(c(header, raw(4)), "first chunk is not ds64")

  # The chunk runs past the end of the file, or gives a table it has no room
  # for.
  refused(c(header, charToRaw("ds64"), mdU32(28), raw(10)), "runs past the end")
  refused(
    c(header, mdChunk("ds64", c(mdU64(1000), mdU64(200), mdU64(100), mdU32(1))), audio),
    "no room"
  )

  # A RIFF size more than the file holds when nothing in it is cut short, as the
  # E2 file of the reference set gives one, and a RIFF size less than the file
  # holds when the audio size was written.
  good <- mdRf64Wav()
  refused(mdRf64Wav(riffSize=length(good)), "8 more than the file holds")
  refused(c(good, raw(16)), "16 bytes at the end of the file")

  # A data size running past the end of a file the RIFF size says is whole.
  refused(mdRf64Wav(dataSize=1000), "says is whole")

  # A data chunk giving a size of its own that the ds64 chunk does not.
  refused(mdRf64(mdFmt(), mdChunk("data", raw(200)), dataSize=100), "data chunk gives")
})

test_that("A chunk that leaves its size to a table without it is refused", {
  # Nothing in the file says how long the chunk is, so it runs past the end of a
  # file the RIFF size says is whole.
  bytes <- mdRf64Wav(after=mdDeferred("JUNK", raw(10)))
  expect_error(.wavChunks(bytes), "says is whole", class="badDs64")
})

test_that("An RF64 recording cut short or never finished is walked as a RIFF one is", {
  # Cut short by a flat battery or a full card, with the sizes still saying what
  # the writer meant: the audio runs past the end of the file.
  good <- mdRf64Wav(after=raw(0))
  info <- .wavChunks(good[seq_len(length(good) - 100)])
  data <- .wavFindChunk(info$chunks, "data")
  expect_true(data$truncated)
  expect_equal(data$size, 100)

  # Never finished: the recorder stopped before writing any size, which leaves
  # the zeros it wrote at the start. The audio after the empty data chunk reads
  # as a chunk running past the end, as it does in a RIFF file.
  unfinished <- mdRf64Wav(dataSize=0, frames=0, riffSize=0, after=raw(0))
  info <- .wavChunks(unfinished)
  expect_equal(info$riffSize, 0)
  expect_equal(.wavFindChunk(info$chunks, "data")$size, 0)
  expect_true(info$chunks[[length(info$chunks)]]$truncated)

  # Audio there can read as a chunk leaving its size to a table without it.
  # That is no reason to refuse a recording that was never finished.
  unfinished <- mdRf64(
    mdFmt(), mdDeferred("data", c(charToRaw("abcd"), mdU32(4294967295), raw(40))),
    dataSize=0, riffSize=0
  )
  info <- .wavChunks(unfinished)
  expect_true(.wavFindChunk(info$chunks, "abcd")$truncated)
})

test_that("A ds64 chunk that miscounts the sample frames is read, and says so", {
  expect_false(.wavChunks(mdRf64Wav(frames=99))$ds64$countAgrees)
  # A count of zero gives none, and so cannot be wrong.
  expect_true(.wavChunks(mdRf64Wav(frames=0))$ds64$countAgrees)

  # A format that is not PCM or floating point takes no fixed number of bytes
  # to a frame, so its count cannot be weighed.
  adpcm <- mdChunk("fmt ", c(
    mdU16(2), mdU16(1), mdU32(48000), mdU32(24000), mdU16(256), mdU16(4)
  ))
  info <- .wavChunks(mdRf64(adpcm, mdDeferred("data", raw(200)), dataSize=200, frames=7))
  expect_true(info$ds64$countAgrees)
  expect_true(is.na(info$ds64$frameSize))

  # WAVE_FORMAT_EXTENSIBLE is weighed as the format it extends.
  extensible <- mdChunk("fmt ", c(
    mdU16(65534), mdU16(1), mdU32(48000), mdU32(96000), mdU16(2), mdU16(16),
    mdU16(22), mdU16(16), mdU32(4), mdU16(1), raw(14)
  ))
  info <- .wavChunks(mdRf64(extensible, mdDeferred("data", raw(200)), dataSize=200, frames=99))
  expect_equal(info$ds64$frameSize, 2)
  expect_false(info$ds64$countAgrees)

  # Nothing can be counted in a file with no data chunk.
  expect_true(.wavChunks(mdRf64(mdFmt(), dataSize=0, frames=5))$ds64$countAgrees)

  # Nor weighed where the format chunk lies beyond the bytes in hand, here
  # because it comes after the audio and only the start of the file is read.
  bytes <- mdRf64(mdDeferred("data", raw(200)), mdFmt(), dataSize=200, frames=99)
  info <- .wavChunks(bytes[1:268], total=length(bytes))
  expect_identical(chunkIds(info), c("ds64", "data", "fmt "))
  expect_true(info$ds64$countAgrees)
  expect_false(.wavChunks(bytes)$ds64$countAgrees)
})

test_that("Walking an RF64 file reads its chunk headers and not its audio", {
  bytes <- mdRf64Wav(samples=rep(500L, 100000))
  bytesRead <- 0
  counting <- function(at, n) {
    bytesRead <<- bytesRead + n
    return(bytes[at:(at + n - 1L)])
  }

  info <- .wavWalk(counting, length(bytes), length(bytes))
  expect_false(is.null(.wavFindChunk(info$chunks, "guan")))
  # The RIFF header, the ds64 chunk, eight bytes for each chunk header and the
  # first sixteen of the format chunk.
  expect_lt(bytesRead, 200)
})

test_that("Sizes are written where each form of file keeps them", {
  riff <- .wavPatch(raw(48), .wavSizePatches("RIFF", NULL, 45, 1000, 900))
  expect_equal(riff[5:8], mdU32(1000))
  expect_equal(riff[41:44], mdU32(900))
  # Without a data size, only the RIFF size is written.
  expect_length(.wavSizePatches("RIFF", NULL, 45, 1000), 1)

  bytes <- mdRf64Wav()
  info <- .wavChunks(bytes)
  start <- .wavFindChunk(info$chunks, "data")$start
  patched <- .wavPatch(bytes, .wavSizePatches(info$form, info$ds64, start, 5000, 150))
  # The headers say the sizes are in the ds64 chunk, which holds them and the
  # number of frames the data now holds.
  expect_equal(patched[5:8], mdU32(4294967295))
  expect_equal(patched[(start - 4):(start - 1)], mdU32(4294967295))
  expect_equal(patched[21:44], c(mdU64(5000), mdU64(150), mdU64(75)))
  expect_length(.wavSizePatches(info$form, info$ds64, start, 5000), 2)

  # Where the format does not say how many bytes a frame takes, the count is
  # given as none rather than left saying what is no longer true.
  info$ds64$frameSize <- NA
  patched <- .wavPatch(bytes, .wavSizePatches(info$form, info$ds64, start, 5000, 150))
  expect_equal(patched[37:44], mdU64(0))
})

test_that("The fields of a format chunk are read only as far as they are held", {
  pcm <- c(mdU16(1), mdU16(2), mdU32(44100), mdU32(176400), mdU16(4), mdU16(16))
  fields <- .wavFmtFields(pcm, 16)
  expect_equal(fields$channels, 2)
  expect_equal(fields$sampleRate, 44100)
  expect_equal(fields$frameSize, 4)
  expect_null(.wavFmtFields(pcm[1:15], 16))

  # WAVE_FORMAT_EXTENSIBLE needs its sub-format to say what it extends.
  extensible <- c(
    mdU16(65534), mdU16(1), mdU32(8000), mdU32(16000), mdU16(2), mdU16(16),
    mdU16(22), mdU16(16), mdU32(4), mdU16(3), raw(14)
  )
  expect_equal(.wavFmtFields(extensible, 40)$format, 3)
  expect_null(.wavFmtFields(extensible[1:16], 40))
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

# A body of an odd number of bytes is followed by a pad byte, and that byte
# belongs to the file. Some writers leave it out, which ended the walk at the
# first such chunk and lost everything behind it.

test_that("A chunk behind an unpadded body is still found", {
  bytes <- mdWav(mdFmt(), mdChunkRagged("data", raw(15)), mdChunk("bext", raw(8)))

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "data", "bext"))
  expect_identical(.wavFindChunk(.wavChunks(bytes)$chunks, "data")$size, 15)
  expect_identical(ids, vapply(.wavChunksFile(mdFile(bytes))$chunks,
                               function(c) c$id, character(1)))
})

test_that("A padded body is read as it always was", {
  # The control for the test above: the same file written correctly, with a
  # body of fifteen bytes, a size of fifteen and a nul pad byte. mdChunk()
  # cannot build it, as it pads the body and then declares the padded length,
  # so what it writes is an even-sized chunk.
  bytes <- c(
    charToRaw("RIFF"), mdU32(4 + 24 + 24 + 16), charToRaw("WAVE"),
    mdFmt(),
    charToRaw("data"), mdU32(15), raw(15), as.raw(0),
    mdChunk("bext", raw(8))
  )

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "data", "bext"))
  expect_identical(.wavFindChunk(.wavChunks(bytes)$chunks, "data")$size, 15)
})

test_that("Padding that is not a nul is still padding where a chunk follows it", {
  # The specification gives the pad byte as zero, and a byte that is not zero
  # where the pad belongs is the reason to suspect it was never written. A
  # writer may pad with something else, so the bytes a step back must look like
  # a chunk before the walk goes there.
  bytes <- c(
    charToRaw("RIFF"), mdU32(4 + 24 + 24 + 16), charToRaw("WAVE"),
    mdFmt(),
    charToRaw("data"), mdU32(15), raw(15), as.raw(255),
    mdChunk("bext", raw(8))
  )

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "data", "bext"))
})

test_that("A printable pad byte is padding, however much it reads as a header", {
  # The test above pads with 255, which cannot begin an identifier. A writer is
  # free to pad with anything, and where the byte is printable the four bytes
  # from it read as an identifier and the four behind them as a size. Where that
  # size happens to fit the rest of the file, a step back used to look right:
  # here "guan" padded with an X reads as a chunk "XLIS" of 1108 bytes, which
  # swallowed the list and the audio behind it.
  bytes <- mdWav(
    mdFmt(),
    c(charToRaw("guan"), mdU32(15), raw(15), charToRaw("X")),
    mdChunk("LIST", charToRaw("INFO")),
    mdChunk("data", raw(1100))
  )

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "guan", "LIST", "data"))
  expect_identical(.wavFindChunk(.wavChunks(bytes)$chunks, "guan")$size, 15)
  expect_identical(ids, vapply(.wavChunksFile(mdFile(bytes))$chunks,
                               function(c) c$id, character(1)))
})

test_that("A pad byte that is not a nul is padding where neither reading gives a chunk", {
  # The remaining case: the byte is not a nul, so the writer did something the
  # format does not allow, but nothing reads as a chunk from either position.
  # There is no evidence to step back on, so the file is read as the padding
  # says and the chunks in front of the fault are kept.
  bytes <- mdWav(
    mdFmt(),
    c(charToRaw("guan"), mdU32(5), charToRaw("GUANO"), charToRaw("X")),
    as.raw(rep(1, 16))
  )

  info <- .wavChunks(bytes)
  ids <- vapply(info$chunks, function(c) c$id, character(1))
  expect_identical(ids[1:2], c("fmt ", "guan"))
  expect_identical(.wavFindChunk(info$chunks, "guan")$size, 5)
})

test_that("An unpadded body is still recovered where the padding's own position reads as a header", {
  # The two readings have to be told apart on more than four printable bytes
  # and a size that fits. Here the pad really was left out, and the position
  # the padding gives falls one byte into the next header: "wamd" with a size
  # of 66 whose body begins with a nul reads there as "amdB" with a size of
  # zero, which fits anywhere. What settles it is that nothing follows that
  # reading - the bytes after it are the wamd body - while the step back gives
  # a chunk the audio follows.
  bytes <- mdWav(
    mdFmt(),
    mdChunkRagged("guan", charToRaw("GUANO|Version:1.0\nMake:X\n")),
    c(charToRaw("wamd"), mdU32(66),
      c(as.raw(0), charToRaw("A Recorder"), raw(55))),
    mdChunk("data", raw(16))
  )

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "guan", "wamd", "data"))
})

test_that("Several unpadded bodies in a row are all walked past", {
  bytes <- mdWav(
    mdFmt(),
    mdChunkRagged("guan", charToRaw("GUANO|Version:1.0\nMake:X\n")),
    mdChunkRagged("wamd", mdWamdField(1L, "A Recorder")),
    mdChunk("data", raw(16))
  )

  ids <- vapply(.wavChunks(bytes)$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "guan", "wamd", "data"))
})

test_that("An unpadded body at the end of the file ends the walk quietly", {
  bytes <- mdWav(mdFmt(), mdChunk("data", raw(16)), mdChunkRagged("bext", raw(7)))

  info <- .wavChunks(bytes)
  ids <- vapply(info$chunks, function(c) c$id, character(1))
  expect_identical(ids, c("fmt ", "data", "bext"))
  expect_identical(.wavFindChunk(info$chunks, "bext")$size, 7)
})

test_that("Metadata written behind an unpadded chunk reaches the record", {
  # What the fault costs: 27 files in a cached copy of the TSA corpus keep a
  # wamd chunk behind an unpadded GUANO chunk, and it was read by nothing.
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunkRagged("guan", charToRaw("GUANO|Version:1.0\nMake:Open Acoustic Devices\n")),
    mdChunk("wamd", mdWamdField(1L, "A Recorder")),
    mdData()
  ))
  m <- readMetadata(f, filename=FALSE)

  expect_identical(names(m$sources), c("guano", "wamd"))
  expect_identical(m$fields$model, "A Recorder")
  expect_length(m$unparsed, 0)
})
