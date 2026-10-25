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
