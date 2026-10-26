# The bundled AUDIOMOTHT.WAV is a triggered recording written at the segment
# size current AudioMoth firmware uses. It was checked, byte for byte, against
# the reference implementation published by Open Acoustic Devices, so the
# figures below describe an expansion known to be correct, and are not simply
# what this package happened to produce. Expanding it exercises the reader
# against a fixed file rather than against this package's own writer.
#
# It holds four segments: a quiet first one, a quiet second one that is omitted,
# a triggered third one, and a short final one. The first and last segments are
# always written, as silence where they fall below the threshold, so the only
# audible part of the expansion is the third segment.
test_that("The bundled triggered recording expands as the reference tool expands it", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  e <- audiomothExpand(f)

  expect_s4_class(e, "Wave")
  expect_equal(length(e@left), 49908)
  expect_equal(e@samp.rate, 8000)
  expect_equal(e@bit, 16)
  expect_false(e@stereo)

  samples <- as.integer(e@left)
  # The first segment and the omitted second segment are both silence.
  expect_true(all(samples[1:32524] == 0))
  # The triggered third segment is the only audio in the file.
  expect_equal(which(samples != 0)[1], 32525)
  expect_equal(max(which(samples != 0)), 48908)
  expect_equal(sum(samples != 0), 16241)
  expect_equal(range(samples), c(-11000, 11000))
  expect_equal(sum(abs(as.numeric(samples))), 56236756)
  # The short final segment is below the threshold, so it too is silence.
  expect_true(all(samples[48909:49908] == 0))
})

test_that("The bundled triggered recording holds one encoded block", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  bytes <- readBin(f, "raw", n=file.size(f))
  nBlocks <- length(bytes) %/% 512L
  blocks <- bytes[seq_len(nBlocks * 512L)]
  dim(blocks) <- c(512L, nBlocks)

  counts <- .audiomothBlockCounts(blocks)
  # One omitted segment of 32768 bytes, which is 64 blocks of 512 bytes.
  expect_equal(sum(counts > 0), 1)
  expect_equal(counts[counts > 0], 64)
})

test_that("The bundled triggered recording reads through readAudio", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  expect_equal(length(readAudio(f)@left), 49908)

  # Read through tuneR it is shorter, as the omitted period is still missing.
  expect_lt(length(tuneR::readWave(f)@left), 49908)
})

test_that("The bundled triggered recording still carries a readable comment", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  d <- audiomothWave(f)
  expect_equal(d$start_time, "21:06:00")
  expect_equal(d$start_date, "14/05/2026")
  expect_equal(d$serial, "247AB4055C7B17DB")
})

# A wave with loud bursts separated by near-silence, long enough at the default
# segment size to produce several omitted periods.
twavFixture <- function(rate=8000, seconds=6, bursts=c(1, 4)) {
  n <- as.integer(rate * seconds)
  set.seed(1)
  samples <- as.integer(round(stats::rnorm(n, sd=20)))
  for (at in bursts) {
    i <- seq.int(as.integer(at * rate) + 1L, length.out=rate %/% 4L)
    samples[i] <- as.integer(round(sin(seq(0, 300, length.out=length(i))) * 12000))
  }
  tuneR::Wave(left=samples, samp.rate=rate, bit=16, pcm=TRUE)
}

test_that("A triggered file expands to the length it was written from", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  audiomothCompress(w, f, threshold=1000, segment=1024)
  expect_lt(file.size(f), 488 + 2 * length(w@left))

  e <- audiomothExpand(f)
  expect_s4_class(e, "Wave")
  expect_equal(length(e@left), length(w@left))
  expect_equal(e@samp.rate, w@samp.rate)
  expect_equal(e@bit, 16)
  expect_false(e@stereo)
})

test_that("Audio above the threshold survives and the rest becomes silence", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  audiomothCompress(w, f, threshold=1000, segment=1024)
  e <- audiomothExpand(f)

  kept <- e@left != 0
  expect_true(any(kept))
  expect_equal(e@left[kept], w@left[kept])
  expect_true(all(abs(w@left[!kept]) < 1000))
})

test_that("Expanding to a file gives the same audio as expanding to a Wave", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))

  audiomothCompress(w, f, threshold=1000, segment=1024)
  expect_equal(audiomothExpand(f, out), out)
  expect_true(file.exists(out))
  expect_equal(tuneR::readWave(out)@left, audiomothExpand(f)@left)
})

test_that("A file with no encoded blocks is returned unchanged", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  # Everything is above a threshold of zero, so nothing is left out.
  audiomothCompress(w, f, threshold=0, segment=1024)
  expect_equal(audiomothExpand(f)@left, w@left)
})

test_that("An ordinary wave file passes through expansion unchanged", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  tuneR::writeWave(w, f)
  expect_equal(audiomothExpand(f)@left, w@left)
})

test_that("Encoded blocks round trip", {
  for (count in c(1, 2, 63, 64, 1000, 2^16, 2^31 + 5)) {
    block <- .audiomothEncodedBlock(count)
    expect_equal(length(block), 512)
    dim(block) <- c(512L, 1L)
    expect_equal(.audiomothBlockCounts(block), count)
  }
})

test_that("A block of silence is not mistaken for an encoded block", {
  block <- raw(512)
  dim(block) <- c(512L, 1L)
  expect_equal(.audiomothBlockCounts(block), 0)

  # Thirty-two values of -1 encode zero skipped blocks, which the reference
  # implementation copies through rather than expanding.
  zero <- writeBin(c(rep(-1L, 32), integer(224)), raw(), size=2L, endian="little")
  dim(zero) <- c(512L, 1L)
  expect_equal(.audiomothBlockCounts(zero), 0)
})

test_that("Segments are sized as the format requires", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  expect_error(audiomothCompress(w, f, threshold=1000, segment=1000), "multiple of 512")
  expect_error(audiomothCompress(w, f, threshold=1000, segment=0), "longer than")

  # The smallest segment the format allows leaves only twelve samples in the
  # first segment, the rest of it being header.
  audiomothCompress(w, f, threshold=1000, segment=512)
  expect_equal(length(audiomothExpand(f)@left), length(w@left))
})

test_that("Only mono 16-bit waves can be written", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  stereo <- tuneR::Wave(left=integer(100), right=integer(100), samp.rate=8000, bit=16)
  expect_error(audiomothCompress(stereo, f, threshold=1), "mono")

  eight <- tuneR::Wave(left=integer(100), samp.rate=8000, bit=8)
  expect_error(audiomothCompress(eight, f, threshold=1), "16-bit")

  expect_error(audiomothCompress("not a wave", f, threshold=1), "Wave object")
  expect_error(audiomothCompress(twavFixture(), f, threshold=-1), "non-negative")

  loud <- tuneR::Wave(left=c(integer(99), 40000L), samp.rate=8000, bit=16)
  expect_error(audiomothCompress(loud, f, threshold=1), "16-bit recording")
})

test_that("Existing files are not replaced without being asked", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))

  audiomothCompress(w, f, threshold=1000, segment=1024)
  expect_error(audiomothCompress(w, f, threshold=1000, segment=1024), "overwrite")
  expect_silent(audiomothCompress(w, f, threshold=1000, segment=1024, overwrite=TRUE))

  audiomothExpand(f, out)
  expect_error(audiomothExpand(f, out), "overwrite")
  expect_equal(audiomothExpand(f, out, overwrite=TRUE), out)

  expect_error(audiomothExpand(tempfile()), "does not exist")
})

test_that("Only mono 16-bit PCM files can be expanded", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  stereo <- tuneR::Wave(
    left = as.integer(stats::rnorm(1000, sd=1000)),
    right = as.integer(stats::rnorm(1000, sd=1000)),
    samp.rate = 8000, bit = 16
  )
  tuneR::writeWave(stereo, f)
  expect_error(audiomothExpand(f), "mono")
})

test_that("readAudio expands triggered recordings", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  audiomothCompress(w, f, threshold=1000, segment=1024)

  whole <- readAudio(f)
  expect_equal(length(whole@left), length(w@left))

  part <- readAudio(f, from=1, to=2, units="seconds")
  expect_equal(length(part@left), 8000, tolerance=2)

  samples <- readAudio(f, from=1, to=100, units="samples")
  expect_equal(length(samples@left), 100)
})

test_that("readAudio still reads an ordinary file named like a triggered one", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  tuneR::writeWave(w, f)
  expect_equal(readAudio(f)@left, w@left)
})

test_that("A wave shorter than one segment is written and read back", {
  # The whole recording fits in the first segment, which also holds the header,
  # so there is nothing that could be left out and no encoded block.
  w <- tuneR::Wave(left=as.integer(rep(c(-5000L, 5000L), 100)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  audiomothCompress(w, f, threshold=1000)
  expect_equal(file.size(f), 488 + 2 * length(w@left))
  expect_equal(audiomothExpand(f)@left, w@left)
})

test_that("A recording cut short is expanded with a warning", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)

  # Cut the file in half, as a flat battery or a full card would.
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) %/% 2)], con)
  close(con)

  expect_warning(wave <- audiomothExpand(f), "truncated")
  expect_s4_class(wave, "Wave")
  expect_gt(length(wave@left), 0)
})

test_that("Files that are not mono 16-bit PCM are rejected", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  eight <- tuneR::Wave(left=as.integer(rep(100L, 2000)), samp.rate=8000, bit=8)
  tuneR::writeWave(eight, f)
  expect_error(audiomothExpand(f), "16-bit")
})

test_that("A comment too long for the header is truncated with a warning", {
  w <- twavFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  expect_warning(
    audiomothCompress(w, f, threshold=1000, segment=1024,
                      comment=strrep("a", 400)),
    "truncated"
  )
  # The header is still the length the format gives it, so the file still
  # expands to the length it was written from.
  expect_equal(length(audiomothExpand(f)@left), length(w@left))
  # The comment was cut to fit, so the header is the length the format gives
  # it and the audio still starts at byte 489.
  bytes <- readBin(f, "raw", n=file.size(f))
  expect_equal(.wavFormat(bytes)$dataStart, 489)
})

test_that("A wave with nothing in it is rejected", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  empty <- tuneR::Wave(left=integer(0), samp.rate=8000, bit=16)
  expect_error(audiomothCompress(empty, f, threshold=1), "no samples")
})

test_that("An encoded block cannot describe an impossible period", {
  expect_error(.audiomothEncodedBlock(0), "between 1")
  expect_error(.audiomothEncodedBlock(2^32), "between 1")
})

test_that("A file that is not a wave file is reported", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  con <- file(f, "wb")
  writeBin(charToRaw(strrep("not a wave file ", 40)), con)
  close(con)
  expect_error(audiomothExpand(f), "RIFF")

  con <- file(f, "wb")
  writeBin(c(charToRaw("RIFF"), .uintToRaw(100, 4L), charToRaw("JUNK"), raw(100)), con)
  close(con)
  expect_error(audiomothExpand(f), "WAVE")

  con <- file(f, "wb")
  writeBin(charToRaw("RIFF"), con)
  close(con)
  expect_error(audiomothExpand(f), "too short")
})

test_that("A wave file that is not PCM is rejected", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  # IEEE float, which is format 3 rather than 1.
  header <- c(
    charToRaw("RIFF"), .uintToRaw(36 + 400, 4L), charToRaw("WAVE"),
    charToRaw("fmt "), .uintToRaw(16L, 4L),
    .uintToRaw(3L, 2L), .uintToRaw(1L, 2L),
    .uintToRaw(8000L, 4L), .uintToRaw(32000L, 4L),
    .uintToRaw(4L, 2L), .uintToRaw(16L, 2L),
    charToRaw("data"), .uintToRaw(400L, 4L)
  )
  con <- file(f, "wb")
  writeBin(c(header, raw(400)), con)
  close(con)

  expect_error(audiomothExpand(f), "PCM")
})

test_that("A wave file with no audio in it is reported", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  # A header with a format chunk but no data chunk at all.
  header <- c(
    charToRaw("RIFF"), .uintToRaw(28 + 512, 4L), charToRaw("WAVE"),
    charToRaw("fmt "), .uintToRaw(16L, 4L),
    .uintToRaw(1L, 2L), .uintToRaw(1L, 2L),
    .uintToRaw(8000L, 4L), .uintToRaw(16000L, 4L),
    .uintToRaw(2L, 2L), .uintToRaw(16L, 2L)
  )
  con <- file(f, "wb")
  writeBin(c(header, charToRaw("LIST"), .uintToRaw(512L, 4L), raw(512)), con)
  close(con)

  expect_error(audiomothExpand(f), "DATA chunk")
})

test_that("A recording of a few samples survives a round trip", {
  # Short enough that the file has no whole 512-byte block after its header,
  # so there is no block that could hold an encoded period.
  w <- tuneR::Wave(left=as.integer(rep(1000L, 10)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))

  audiomothCompress(w, f, threshold=1)
  expect_lt(file.size(f), 512)

  expect_equal(audiomothExpand(f)@left, w@left)
  audiomothExpand(f, out)
  expect_equal(tuneR::readWave(out)@left, w@left)
})

test_that("A size too large for a WAV header is refused rather than wrapped", {
  expect_error(.uintToRaw(2^32, 4L), "does not fit")
  expect_error(.uintToRaw(-1, 4L), "does not fit")
  expect_error(.uintToRaw(2^16, 2L), "does not fit")
  expect_equal(.uintToRaw(2^32 - 1, 4L), as.raw(c(255, 255, 255, 255)))
})

test_that("A file this package cannot expand is told apart from a failure to expand", {
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  stereo <- tuneR::Wave(
    left = as.integer(stats::rnorm(1000, sd=1000)),
    right = as.integer(stats::rnorm(1000, sd=1000)),
    samp.rate = 8000, bit = 16
  )
  tuneR::writeWave(stereo, f)

  # readAudio() reads such a file by its ordinary means rather than failing.
  expect_s3_class(
    tryCatch(audiomothExpand(f), error=function(e) e),
    "unsupportedWav"
  )
  read <- readAudio(f)
  expect_true(read@stereo)
  expect_equal(read@left, stereo@left)
})

test_that("A header longer than the first block is still read", {
  # The AudioMoth header is 488 bytes, but the application note warns that the
  # comment and artist fields may grow, which would put the data chunk beyond
  # the first 512-byte block.
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  header <- c(
    charToRaw("RIFF"), .uintToRaw(564L, 4L), charToRaw("WAVE"),
    charToRaw("fmt "), .uintToRaw(16L, 4L),
    .uintToRaw(1L, 2L), .uintToRaw(1L, 2L),
    .uintToRaw(8000L, 4L), .uintToRaw(16000L, 4L),
    .uintToRaw(2L, 2L), .uintToRaw(16L, 2L),
    charToRaw("LIST"), .uintToRaw(500L, 4L), raw(500),
    charToRaw("data"), .uintToRaw(20L, 4L)
  )
  con <- file(f, "wb")
  writeBin(c(header, writeBin(rep(777L, 10), raw(), size=2L, endian="little")), con)
  close(con)

  expect_equal(as.integer(audiomothExpand(f)@left), rep(777L, 10))
})

test_that("A format chunk reaching past the blocks read is still read", {
  # Expanding reads the whole 512-byte blocks first, as the omitted periods are
  # found in them, and the rest of the file only if the header turns out not to
  # fit. A format chunk can then begin inside the blocks and end outside them.
  # Reading past the end of a raw vector gives zero bytes rather than failing,
  # so the fields would come back as zeros and this recording, which is mono
  # 16-bit PCM, would be refused for being none of those.
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  # The data chunk is put first so that it has been found by the time the walk
  # stops, and the filler sets the format chunk 1009 bytes in: its header is
  # inside the first two blocks and its body runs past them.
  audio <- writeBin(rep(777L, 8), raw(), size=2L, endian="little")
  bytes <- c(
    charToRaw("RIFF"), .uintToRaw(0L, 4L), charToRaw("WAVE"),
    charToRaw("data"), .uintToRaw(length(audio), 4L), audio,
    charToRaw("LIST"), .uintToRaw(964L, 4L), rep(charToRaw("x"), 964),
    charToRaw("fmt "), .uintToRaw(16L, 4L),
    .uintToRaw(1L, 2L), .uintToRaw(1L, 2L), .uintToRaw(8000L, 4L),
    .uintToRaw(16000L, 4L), .uintToRaw(2L, 2L), .uintToRaw(16L, 2L),
    raw(100)
  )
  fmt <- .wavFindChunk(
    .wavChunks(bytes[seq_len(1024L)], total=length(bytes))$chunks, "fmt "
  )
  expect_equal(fmt$start, 1017)
  expect_gt(fmt$start + 15L, 1024L)

  con <- file(f, "wb")
  writeBin(bytes, con)
  close(con)

  expect_equal(as.integer(audiomothExpand(f)@left), rep(777L, 8))
})

test_that("A chunk after the audio is not mistaken for an omitted period", {
  w <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))
  audiomothCompress(w, f, threshold=1)

  # Put a chunk after the audio whose contents read exactly as an encoded
  # block, aligned so that it falls on a 512-byte boundary of the file.
  bytes <- readBin(f, "raw", n=file.size(f))
  payload <- .audiomothEncodedBlock(2)
  padding <- raw((512 - (length(bytes) + 8) %% 512) %% 512)
  bytes <- c(
    bytes,
    charToRaw("padd"), .uintToRaw(length(padding), 4L), padding,
    charToRaw("junk"), .uintToRaw(length(payload), 4L), payload
  )
  bytes[5:8] <- .uintToRaw(length(bytes) - 8, 4L)
  con <- file(f, "wb")
  writeBin(bytes, con)
  close(con)

  # The audio is unchanged, and the chunk is carried over rather than expanded.
  expect_equal(length(audiomothExpand(f)@left), length(w@left))
  audiomothExpand(f, out)
  ids <- vapply(.wavChunks(readBin(out, "raw", n=file.size(out)))$chunks,
                function(chunk) chunk$id, character(1))
  expect_true(all(c("data", "junk") %in% ids))
})

test_that("A recording cut short is given sizes that match the file written", {
  w <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))

  audiomothCompress(w, f, threshold=1)
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) - 5000)], con)
  close(con)

  suppressWarnings(audiomothExpand(f, out))

  # The sizes a truncated recording declares were never true, so they have to
  # describe the file as written rather than as it said it would be.
  written <- readBin(out, "raw", n=file.size(out))
  info <- .wavChunks(written)
  expect_equal(info$riffSize + 8, length(written))
  data <- .wavFindChunk(info$chunks, "data")
  expect_lte(data$start + data$size - 1, length(written))
})

test_that("A sample range past the end stops at the end of the recording", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  wave <- readAudio(f, from=49900, to=50000, units="samples")

  expect_equal(length(wave@left), 9)
  expect_false(any(is.na(wave@left)))
})

test_that("Windowing a triggered recording covers all of it", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")

  starts <- c()
  invisible(windowing(
    f, window.length=8000, window.overlap=0,
    FUN=function(wave, start, window.length) {
      starts <<- c(starts, start)
      length(wave@left)
    }
  ))

  # The recording expands to 49908 samples, so six whole windows of 8000 fit.
  # Scheduling from the file on disk gives only four, as it holds 33780
  # samples with the periods that did not trigger a recording left out.
  expect_equal(starts, c(1, 8001, 16001, 24001, 32001, 40001))

  # The audible part of this recording runs from sample 32525, which the four
  # windows the file on disk would give never reach.
  expect_gt(max(starts) + 8000 - 1, 32525)

  lengths <- unlist(windowing(
    f, window.length=8000, window.overlap=0,
    FUN=function(wave, start, window.length) length(wave@left)
  ))
  expect_true(all(lengths == 8000))
})

test_that("Windowing a triggered recording to its end stops at the last sample", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")

  lengths <- unlist(windowing(
    f, window.length=8000, window.overlap=0, complete.windows=FALSE,
    FUN=function(wave, start, window.length) length(wave@left)
  ))
  expect_equal(length(lengths), 7)
  # The last window runs past the end of the recording and stops at it.
  expect_equal(sum(lengths), 49908)
})

test_that("A triggered recording reads from a start point with no end", {
  f <- system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")

  # readAudio()'s `to` defaults to Inf, which seewave::cutw() will not take as
  # an end point, so a triggered recording has to be cut through the same
  # helper as every other format rather than be given Inf.
  wave <- readAudio(f, from=1)
  expect_equal(length(wave@left), 41908)
  expect_false(any(is.na(wave@left)))

  expect_equal(length(readAudio(f, from=1, to=2)@left), 8000)
  expect_equal(length(readAudio(f, from=0, to=10)@left), 49908)
})

test_that("Metadata is carried onto a section of a triggered recording", {
  w <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1, guano=guanoCreate(Make="Open Acoustic Devices"))

  expect_identical(guano(readAudio(f))[["Make"]], "Open Acoustic Devices")
  expect_identical(guano(readAudio(f, from=1, to=2))[["Make"]], "Open Acoustic Devices")
  expect_identical(guano(readAudio(f, from=1, to=100, units="samples"))[["Make"]], "Open Acoustic Devices")
})

test_that("A file that could not be written is reported rather than returned", {
  w <- tuneR::Wave(left=as.integer(rep(1000L, 10)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1)

  # Short enough to need no expansion, which is the path that could hand back
  # the name of a file it never wrote. It fails as the expanding path does,
  # when the file cannot be opened to write.
  expect_lt(file.size(f), 512)
  missing <- file.path(tempfile(), "not", "there.WAV")
  expect_error(suppressWarnings(audiomothExpand(f, missing)))
  expect_false(file.exists(missing))

  # And a recording long enough to be expanded fails in the same way.
  long <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  g <- tempfile(fileext="T.WAV")
  on.exit(unlink(g), add=TRUE)
  audiomothCompress(long, g, threshold=1)
  expect_error(suppressWarnings(audiomothExpand(g, missing)))
  expect_false(file.exists(missing))
})

test_that("A recording cut short keeps no size it never had, however short", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 1000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  out <- tempfile(fileext=".WAV")
  on.exit(unlink(c(f, out)))
  audiomothCompress(w, f, threshold=1)

  # Cut it to the header plus ten samples, leaving a file that declares far
  # more audio than it holds and is too short to hold a whole block of it.
  bytes <- readBin(f, "raw", n=file.size(f))
  con <- file(f, "wb")
  writeBin(bytes[1:508], con)
  close(con)

  suppressWarnings(audiomothExpand(f, out))
  written <- readBin(out, "raw", n=file.size(out))
  info <- .wavChunks(written)
  data <- .wavFindChunk(info$chunks, "data")

  expect_equal(info$riffSize + 8, length(written))
  expect_equal(data$size, 20)
  expect_equal(data$start + data$size - 1, length(written))
})

test_that("A recording cut short is not refused for the size it claims", {
  w <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1)

  # Declare a RIFF size near the largest a WAV header can hold, then cut the
  # file short, as a recording that stopped before it finished would.
  bytes <- readBin(f, "raw", n=file.size(f))
  bytes[5:8] <- .uintToRaw(2^32 - 100, 4L)
  con <- file(f, "wb")
  writeBin(bytes[seq_len(length(bytes) - 5000)], con)
  close(con)

  # What matters is the size about to be written, not the one it claims.
  wave <- suppressWarnings(audiomothExpand(f))
  expect_gt(length(wave@left), 0)
  expect_lt(length(wave@left), length(w@left))
})

test_that("A plan for a recording of billions of bytes is counted in doubles", {
  # 4194304 blocks is 2GiB, beyond what the product of two whole numbers can
  # be counted in, which would otherwise give NA and refuse the recording.
  plan <- .audiomothExpandPlan(numeric(4194304L), 0)
  expect_equal(plan$total, 2147483648)
  expect_false(any(is.na(plan$to)))
})

test_that("Windowing a file that only looks like a triggered recording still works", {
  # The naming convention is only a convention, and an ordinary recording can
  # meet it: anything ending in t.wav does. What marks a triggered recording
  # is the trigger its comment records, which such a file does not have.
  channels <- matrix(as.integer(stats::rnorm(4 * 8000, sd=1000)), ncol=4)
  colnames(channels) <- tuneR::MCnames$name[1:4]
  mc <- tuneR::WaveMC(channels, samp.rate=8000, bit=16)
  f <- file.path(tempdir(), "test.wav")
  on.exit(unlink(f))
  tuneR::writeWave(mc, f)

  starts <- unlist(windowing(f, window.length=1000, window.overlap=0,
                             FUN=function(wave, start, window.length) start))
  expect_equal(length(starts), 8)
})

test_that("A recording can be expanded over itself whatever its length", {
  expandInPlace <- function(wave) {
    f <- tempfile(fileext="T.WAV")
    on.exit(unlink(f))
    audiomothCompress(wave, f, threshold=1)
    audiomothExpand(f, output=f, overwrite=TRUE)
    length(audiomothExpand(f)@left)
  }

  # A recording short enough to need no expansion is copied rather than
  # written, and a file cannot be copied over itself.
  short <- tuneR::Wave(left=as.integer(rep(1000L, 10)), samp.rate=8000, bit=16)
  expect_equal(expandInPlace(short), 10)

  long <- tuneR::Wave(left=as.integer(rep(c(-9000L, 9000L), 16384)), samp.rate=8000, bit=16)
  expect_equal(expandInPlace(long), 32768)
})

test_that("A triggered recording is known by its comment, not its name", {
  w <- twavFixture()

  # Written with a threshold, the comment records it, as a device records it.
  triggered <- file.path(tempdir(), "nothing_like_the_convention.dat")
  on.exit(unlink(triggered))
  audiomothCompress(w, triggered, threshold=1000, segment=1024)
  expect_true(.isTriggeredRecording(triggered))
  header <- readBin(triggered, "raw", n=512)
  expect_true(grepl(
    "Amplitude threshold was 1000",
    rawToChar(header[as.integer(header) != 0L]),
    fixed = TRUE, useBytes = TRUE
  ))

  # An ordinary recording whose name meets the convention is not one.
  ordinary <- file.path(tempdir(), "notatriggeredrecordingT.WAV")
  on.exit(unlink(ordinary), add=TRUE)
  tuneR::writeWave(w, ordinary)
  expect_false(.isTriggeredRecording(ordinary))
  expect_equal(readAudio(ordinary)@left, w@left)

  # Written with no threshold, nothing was left out and nothing is recorded.
  untriggered <- tempfile(fileext="T.WAV")
  on.exit(unlink(untriggered), add=TRUE)
  audiomothCompress(w, untriggered, threshold=0, segment=1024)
  expect_false(.isTriggeredRecording(untriggered))

  # A file that is not there, or is far too short, is not one either.
  expect_false(.isTriggeredRecording(tempfile()))
})

test_that("A triggered recording is expanded whatever it is called", {
  w <- twavFixture()
  f <- file.path(tempdir(), "renamed_by_someone.wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)

  # The file on disk holds less than the recording it stands for.
  expect_lt(length(tuneR::readWave(f)@left), length(w@left))
  # Read through readAudio() it is expanded, although its name says nothing.
  expect_equal(length(readAudio(f)@left), length(w@left))
})

test_that("A comment or artist that is not a single string is refused", {
  w <- twavFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))

  # Each goes into a field holding one string, and anything else would be
  # compared and written a piece at a time.
  expect_error(audiomothCompress(w, f, threshold=1, comment=c("a", "b")), "single string")
  expect_error(audiomothCompress(w, f, threshold=1, comment=character(0)), "single string")
  expect_error(audiomothCompress(w, f, threshold=1, comment=NA), "single string")
  expect_error(audiomothCompress(w, f, threshold=1, artist=c("a", "b")), "single string")

  # A number is a single value and is written as the text of it.
  expect_silent(audiomothCompress(w, f, threshold=1, comment=42, overwrite=TRUE))
})

test_that("A comment that already records the trigger is not made to say it twice", {
  w <- twavFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))

  audiomothCompress(
    w, f, threshold=1000, segment=1024,
    comment="Recorded by a device. Amplitude threshold was 1000."
  )
  header <- readBin(f, "raw", n=512)
  text <- rawToChar(header[as.integer(header) != 0L])
  expect_equal(
    lengths(regmatches(text, gregexpr("Amplitude threshold was", text, useBytes=TRUE))),
    1L
  )
  expect_true(.isTriggeredRecording(f))
})

test_that("Arguments that name one thing are refused several", {
  w <- twavFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1, segment=1024)

  expect_error(audiomothCompress(w, tempfile(), threshold=1, segment=c(1024, 2048)), "single number")
  expect_error(audiomothCompress(w, tempfile(), threshold=1, segment="1024"), "single number")
  expect_error(audiomothExpand(c(f, f)), "single string")
  expect_error(audiomothExpand(f, output=c(tempfile(), tempfile())), "single string")
})

# A recording with periods genuinely left out, so that failing to recognise it
# shows as audio at the wrong times rather than as nothing at all.
omittedFixture <- function() {
  samples <- as.integer(rep(50L, 20000))
  samples[8000:9000] <- 12000L
  tuneR::Wave(left=samples, samp.rate=8000, bit=16)
}

test_that("A trigger recorded beyond the comment field is not counted", {
  w <- omittedFixture()
  f <- tempfile(fileext="T.WAV")
  on.exit(unlink(f))

  # The comment says the threshold, but only past the bytes the field holds,
  # so what is written does not say it and the threshold has to be added.
  expect_warning(
    audiomothCompress(w, f, threshold=1000, segment=1024,
                      comment=paste0(strrep("x", 384), " Amplitude threshold was 1000.")),
    "truncated"
  )
  expect_true(.isTriggeredRecording(f))
  expect_lt(length(tuneR::readWave(f)@left), length(w@left))
  expect_equal(length(readAudio(f)@left), length(w@left))
})

test_that("A triggered recording is expanded whatever its name says", {
  w <- omittedFixture()
  for (ext in c(".dat", "", ".bin", ".mp3")) {
    f <- file.path(tempdir(), paste0("recording", ext))
    unlink(f)
    audiomothCompress(w, f, threshold=1000, segment=1024)

    # The type is guessed from the name, and a triggered recording can be
    # called anything, so the trigger is looked for whatever the name says.
    expect_true(.isTriggeredRecording(f))
    expect_lt(length(tuneR::readWave(f)@left), length(w@left))
    expect_equal(length(readAudio(f)@left), length(w@left))
    unlink(f)
  }
})

test_that("Files that are not triggered recordings read as they did", {
  w <- omittedFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)
  expect_equal(length(readAudio(f)@left), length(w@left))

  expect_gt(length(readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"))@left), 0)
  expect_gt(length(readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))@left), 0)
})

test_that("Trigger wording outside the comment does not make a recording one", {
  w <- omittedFixture()

  # The artist field is text of its own and says nothing about the recording.
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=0, segment=1024, artist="Frequency trigger")
  expect_false(.isTriggeredRecording(f))
  expect_equal(length(readAudio(f)@left), length(w@left))

  # Nor does any other field of the INFO list.
  g <- tempfile(fileext=".wav")
  on.exit(unlink(g), add=TRUE)
  tuneR::writeWave(w, g)
  bytes <- readBin(g, "raw", n=file.size(g))
  note <- charToRaw("Amplitude threshold was 99 in a note, not the comment")
  if (length(note) %% 2) note <- c(note, as.raw(0))
  chunk <- c(
    charToRaw("LIST"), .uintToRaw(length(note) + 12, 4L), charToRaw("INFO"),
    charToRaw("ISBJ"), .uintToRaw(length(note), 4L), note
  )
  bytes <- c(bytes[1:36], chunk, bytes[37:length(bytes)])
  bytes[5:8] <- .uintToRaw(length(bytes) - 8, 4L)
  con <- file(g, "wb")
  writeBin(bytes, con)
  close(con)
  expect_false(.isTriggeredRecording(g))

  # The comment itself still counts, for a file this package writes and for
  # the bundled recording from a device.
  p <- tempfile(fileext=".wav")
  on.exit(unlink(p), add=TRUE)
  audiomothCompress(w, p, threshold=1000, segment=1024)
  expect_true(.isTriggeredRecording(p))
  expect_true(.isTriggeredRecording(
    system.file("extdata/AUDIOMOTHT.WAV", package="sonicscrewdriver")
  ))
  expect_false(.isTriggeredRecording(
    system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  ))
})

test_that("A comment of characters wider than a byte is cut between them", {
  w <- omittedFixture()

  # Cutting at the byte would leave a part of a character, which is no longer
  # text and raises rather than giving the shortened comment that was meant.
  for (n in c(100, 190, 200, 400)) {
    f <- tempfile(fileext=".wav")
    suppressWarnings(
      audiomothCompress(w, f, threshold=1, segment=1024, comment=strrep("\u00e9", n))
    )
    expect_true(.isTriggeredRecording(f))
    expect_equal(length(readAudio(f)@left), length(w@left))
    unlink(f)
  }

  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  expect_warning(
    audiomothCompress(w, f, threshold=0, segment=1024, artist=strrep("\u00e9", 40)),
    "truncated"
  )
  expect_true(file.exists(f))
})

test_that("Text is measured as the bytes it will be written as", {
  w <- omittedFixture()

  # A string held in another encoding counts fewer bytes than it takes once
  # converted, so it could pass a check against a field it does not fit.
  latin <- iconv(strrep("\u00e9", 20), "UTF-8", "latin1")
  expect_equal(nchar(latin, type="bytes"), 20)
  expect_equal(nchar(enc2utf8(latin), type="bytes"), 40)

  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  suppressWarnings(audiomothCompress(w, f, threshold=1, segment=1024, artist=latin))
  expect_true(.isTriggeredRecording(f))
  expect_equal(length(readAudio(f)@left), length(w@left))

  g <- tempfile(fileext=".wav")
  on.exit(unlink(g), add=TRUE)
  suppressWarnings(audiomothCompress(
    w, g, threshold=1, segment=1024,
    comment=iconv(strrep("\u00e9", 300), "UTF-8", "latin1")
  ))
  expect_true(.isTriggeredRecording(g))
})

test_that("A recording already written is not lost to a write that fails", {
  w <- omittedFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1, segment=1024)
  before <- file.size(f)

  # Opening a file to write empties it, so what can fail has to fail first.
  expect_error(
    audiomothCompress(w, f, threshold=1, segment=1024, comment=c("a", "b"), overwrite=TRUE),
    "single string"
  )
  expect_equal(file.size(f), before)
  expect_equal(length(audiomothExpand(f)@left), length(w@left))
})

test_that("The comment is found wherever the file holds it", {
  w <- omittedFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)

  # Put a large chunk before the list, so the comment sits well past the start
  # of the file, where reading a fixed run of bytes would never reach it.
  bytes <- readBin(f, "raw", n=file.size(f))
  filler <- charToRaw(as.character(guanoCreate(Make="X", Note=strrep("z", 6000))))
  chunk <- c(charToRaw("guan"), .uintToRaw(length(filler), 4L), filler)
  bytes <- c(bytes[1:36], chunk, bytes[37:length(bytes)])
  bytes[5:8] <- .uintToRaw(length(bytes) - 8, 4L)
  con <- file(f, "wb")
  writeBin(bytes, con)
  close(con)

  expect_gt(.wavFindChunk(.wavChunks(bytes)$chunks, "LIST")$start, 4096)
  expect_true(.isTriggeredRecording(f))
})

test_that("The comment is found behind another list", {
  w <- omittedFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)
  bytes <- readBin(f, "raw", n=file.size(f))

  # A file may hold a list of cue labels as well as one of information, in
  # either order, and a list of information need not hold a comment. Each
  # inserted list takes 512 bytes, so the omitted periods stay where they are.
  insert <- function(kind, extra=raw(0)) {
    body <- c(charToRaw(kind), extra)
    body <- c(body, raw(512 - 8 - length(body)))
    chunk <- c(charToRaw("LIST"), .uintToRaw(length(body), 4L), body)
    with <- c(bytes[1:36], chunk, bytes[37:length(bytes)])
    with[5:8] <- .uintToRaw(length(with) - 8, 4L)
    path <- tempfile(fileext=".wav")
    con <- file(path, "wb")
    writeBin(with, con)
    close(con)
    path
  }

  labels <- insert("adtl")
  on.exit(unlink(labels), add=TRUE)
  expect_true(.isTriggeredRecording(labels))
  expect_equal(length(readAudio(labels)@left), length(w@left))

  noComment <- insert("INFO", c(charToRaw("IART"), .uintToRaw(8L, 4L),
                                charToRaw("someone"), as.raw(0)))
  on.exit(unlink(noComment), add=TRUE)
  expect_true(.isTriggeredRecording(noComment))
  expect_equal(length(readAudio(noComment)@left), length(w@left))

  # And a recording that is not a triggered one is still not taken for one.
  ordinary <- tempfile(fileext=".wav")
  on.exit(unlink(ordinary), add=TRUE)
  tuneR::writeWave(w, ordinary)
  expect_false(.isTriggeredRecording(ordinary))
})

test_that("A write that fails part way leaves the recording as it was", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 20000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)
  size <- file.size(f)

  # One write to the file is let through before the stop, so the file being
  # written is left part written: stopping before any of it would end the call
  # before it opened a file at all, and show nothing about what is left on
  # disk. These functions also build bytes in memory with the same function,
  # which is why only writes to a connection are stopped.
  expect_equal(
    interrupted(audiomothCompress(w, f, threshold=1, segment=1024, overwrite=TRUE)),
    "interrupted"
  )
  expect_equal(file.size(f), size)
  expect_equal(length(audiomothExpand(f)@left), 20000)

  # Expanding a recording over itself is the same hazard.
  expect_equal(interrupted(audiomothExpand(f, output=f, overwrite=TRUE)), "interrupted")
  expect_equal(file.size(f), size)
  expect_equal(length(audiomothExpand(f)@left), 20000)
  expect_length(list.files(dirname(f), pattern="[.]part$"), 0)
})

test_that("Something that is not a file is not a triggered recording", {
  # A directory is reported as a file of some size on some platforms, where
  # opening one to read raises rather than giving nothing.
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive=TRUE))
  expect_false(.isTriggeredRecording(d))
  expect_false(.isTriggeredRecording(tempfile()))
})

test_that("Truncation says what it did when characters are wider than a byte", {
  w <- twavFixture()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))

  # Three-byte characters cannot fill 32 bytes exactly, so 30 are kept, and
  # the warning says the field it fits within rather than a count it did not
  # truncate to.
  expect_equal(nchar(.cutToBytes(strrep("\u20ac", 20), 32), type="bytes"), 30)
  expect_warning(
    audiomothCompress(w, f, threshold=0, segment=1024, artist=strrep("\u20ac", 20)),
    "within 32 bytes"
  )
})

test_that("Reading closes the recording however it ends", {
  w <- tuneR::Wave(left=as.integer(rep(500L, 20000)), samp.rate=8000, bit=16)
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  audiomothCompress(w, f, threshold=1000, segment=1024)

  # A read that fails, or that someone cancels, would otherwise leave the
  # connection open, which holds the recording open on systems that mind.
  for (ending in list(quote(stop("read failed")),
                      quote(stop(structure(class=c("interrupt", "condition"), list()))))) {
    invisible(gc())
    open <- length(getAllConnections())
    trace(readBin,
          tracer=bquote(if (exists(".interruptRead", .GlobalEnv)) .(ending)),
          print=FALSE)
    assign(".interruptRead", TRUE, .GlobalEnv)
    invisible(tryCatch(audiomothExpand(f), condition=function(c) NULL))
    rm(".interruptRead", envir=.GlobalEnv)
    untrace(readBin)
    expect_equal(length(getAllConnections()), open)
  }

  expect_equal(length(audiomothExpand(f)@left), 20000)
})
