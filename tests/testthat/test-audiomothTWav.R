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
