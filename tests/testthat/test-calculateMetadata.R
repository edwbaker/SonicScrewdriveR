extdata <- function(name) {
  system.file("extdata", name, package="sonicscrewdriver")
}

absolute <- function(path) {
  normalizePath(path, winslash="/", mustWork=FALSE)
}

calculated <- function(w) {
  calculateMetadata(w)@metadata$calculatedMetadata
}

test_that("readAudio records the file it read, whichever reader read it", {
  for (name in c("AUDIOMOTH.WAV", "AUDIOMOTHT.WAV", "AUDIOMOTH.mp3")) {
    expect_identical(attr(readAudio(extdata(name)), "audioFile"), absolute(extdata(name)))
  }
  skip_if_not_installed("av")
  expect_identical(
    attr(readAudio(extdata("AUDIOMOTH.flac")), "audioFile"),
    absolute(extdata("AUDIOMOTH.flac"))
  )
})

test_that("readAudio records the same file for a part of it", {
  f <- extdata("AUDIOMOTH.WAV")
  expect_identical(attr(readAudio(f, from=1, to=2), "audioFile"), absolute(f))
})

test_that("readAudio records a file read by a relative path as an absolute one", {
  dir <- withr::local_tempdir()
  file.copy(extdata("AUDIOMOTH.WAV"), file.path(dir, "copy.wav"))
  w <- withr::with_dir(dir, readAudio("copy.wav"))
  expect_identical(attr(w, "audioFile"), absolute(file.path(dir, "copy.wav")))
})

test_that("tagWave moves the file and its metadata into the metadata slot", {
  f <- extdata("AUDIOMOTH_GUANO.WAV")
  w <- readAudio(f)
  audioMetadata(w) <- readMetadata(f)
  tw <- tagWave(w)
  expect_identical(tw@metadata$audioFile, absolute(f))
  expect_identical(tw@metadata$audioMetadata, attr(w, "audioMetadata"))
  expect_identical(readMetadata(tw), readMetadata(w))
})

test_that("calculateMetadata gives every field of a wave file", {
  f <- extdata("AUDIOMOTH.WAV")
  w <- readAudio(f)
  tw <- calculateMetadata(w)
  expect_s4_class(tw, "TaggedWave")
  m <- tw@metadata$calculatedMetadata
  expect_named(m, c(
    "hash", "hash_method", "wave_hash", "wave_hash_method", "frames",
    "duration", "sample_rate", "channels", "bit_depth", "bit_rate", "codec",
    "bytes", "at", "by"
  ))
  expect_identical(m$hash, digest::digest(file=f, algo="sha256"))
  expect_identical(m$hash_method, "sha256")
  expect_identical(m$wave_hash, sonicscrewdriver:::.waveHash(w, "full"))
  expect_identical(m$wave_hash_method, "full")
  expect_equal(m$frames, length(w@left))
  expect_equal(m$duration, length(w@left) / w@samp.rate)
  expect_equal(m$sample_rate, w@samp.rate)
  expect_equal(m$channels, 1)
  expect_equal(m$bit_depth, 16)
  expect_equal(m$bit_rate, w@samp.rate * 16)
  expect_identical(m$codec, "pcm_s16le")
  expect_equal(m$bytes, file.size(f))
  expect_s3_class(m$at, "POSIXct")
  expect_identical(attr(m$at, "tzone"), "UTC")
  expect_identical(
    m$by, paste("sonicscrewdriver", getNamespaceVersion("sonicscrewdriver"))
  )
})

test_that("calculateMetadata counts the channels of a stereo file", {
  m <- calculated(readAudio(extdata("STEREO.WAV")))
  expect_equal(m$channels, 2)
  expect_equal(m$bit_rate, m$sample_rate * m$bit_depth * 2)
})

test_that("calculateMetadata gives the fields of a file of more than two channels", {
  f <- withr::local_tempfile(fileext=".wav")
  samples <- matrix(round(1000 * sin(1:3000 / 7)), ncol=3)
  colnames(samples) <- c("FL", "FR", "FC")
  tuneR::writeWave(tuneR::WaveMC(samples, samp.rate=8000, bit=16), f, extensible=TRUE)
  tw <- calculateMetadata(readAudio(f))
  expect_s4_class(tw, "TaggedWaveMC")
  m <- tw@metadata$calculatedMetadata
  expect_equal(m$frames, 1000)
  expect_equal(m$channels, 3)
  expect_identical(m$codec, "pcm_s16le")
  expect_equal(m$bit_rate, 8000 * 16 * 3)
  expect_identical(m$hash, digest::digest(file=f, algo="sha256"))
})

test_that("calculateMetadata names floating-point and eight-bit samples as FFmpeg does", {
  f <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(tuneR::Wave(left=sin(1:800 / 10) / 2, samp.rate=8000, bit=32, pcm=FALSE), f)
  m <- calculated(readAudio(f))
  expect_identical(m$codec, "pcm_f32le")
  expect_equal(m$bit_depth, 32)
  expect_equal(m$bit_rate, 8000 * 32)

  f <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(tuneR::Wave(left=round(128 + 100 * sin(1:800 / 10)), samp.rate=8000, bit=8), f)
  m <- calculated(readAudio(f))
  expect_identical(m$codec, "pcm_u8")
  expect_equal(m$bit_depth, 8)
  expect_equal(m$bit_rate, 8000 * 8)
})

test_that("calculateMetadata asks av how a file that is not a wave file is coded", {
  skip_if_not_installed("av")
  m <- calculated(readAudio(extdata("AUDIOMOTH.mp3")))
  expect_identical(m$codec, "mp3")
  expect_true(is.na(m$bit_depth))
  expect_equal(m$bit_rate, av::av_media_info(extdata("AUDIOMOTH.mp3"))$audio$bitrate)
  expect_equal(m$bytes, file.size(extdata("AUDIOMOTH.mp3")))

  w <- readAudio(extdata("AUDIOMOTH.flac"))
  m <- calculated(w)
  expect_identical(m$codec, "flac")
  expect_equal(m$bit_depth, w@bit)
})

test_that("the decoders av reports are named by their codecs, as ffprobe names them", {
  expect_identical(
    sonicscrewdriver:::.ffmpegCodec(c("mp3float", "aac_fixed", "libopus", "flac", "pcm_f32le")),
    c("mp3", "aac", "opus", "flac", "pcm_f32le")
  )
})

test_that("only codecs that keep their samples give a bit depth", {
  lossless <- sonicscrewdriver:::.losslessCodec
  expect_true(lossless("pcm_s24le"))
  expect_true(lossless("flac"))
  expect_false(lossless("mp3"))
  expect_false(lossless("pcm_alaw"))
  expect_false(lossless("pcm_mulaw"))
})

test_that("calculateMetadata names an MP3 file without av, and leaves its bit rate unknown", {
  w <- readAudio(extdata("AUDIOMOTH.mp3"))
  local_mocked_bindings(package.installed = function(...) FALSE)
  m <- calculated(w)
  expect_identical(m$codec, "mp3")
  expect_true(is.na(m$bit_rate))
  expect_true(is.na(m$bit_depth))
  expect_identical(m$hash, digest::digest(file=extdata("AUDIOMOTH.mp3"), algo="sha256"))
})

test_that("calculateMetadata asks av of a wave file whose format chunk cannot be read", {
  skip_if_not_installed("av")
  w <- readAudio(extdata("AUDIOMOTH.WAV"))
  local_mocked_bindings(.wavChunkFromFile = function(...) NULL)
  m <- calculated(w)
  expect_identical(m$codec, "pcm_s16le")
  expect_equal(m$bit_depth, 16)
})

test_that("the same audio in two formats has one wave hash and two file hashes", {
  skip_if_not_installed("av")
  # Each FLAC holds, by the MD5 its own header stores, exactly the audio of the
  # wave file beside it.
  for (name in c("AUDIOMOTH", "STEREO")) {
    wav <- calculated(readAudio(extdata(paste0(name, ".WAV"))))
    flac <- calculated(readAudio(extdata(paste0(name, ".flac"))))
    expect_identical(flac$wave_hash, wav$wave_hash, label=name)
    expect_false(identical(flac$hash, wav$hash), label=name)
  }
})

test_that("calculateMetadata gives no file fields for a wave that was not read from a file", {
  w <- tuneR::sine(440, duration=1000, samp.rate=8000, bit=16, pcm=TRUE)
  m <- calculated(w)
  expect_true(is.na(m$hash))
  expect_true(is.na(m$hash_method))
  expect_true(is.na(m$codec))
  expect_true(is.na(m$bit_rate))
  expect_true(is.na(m$bytes))
  expect_equal(m$bit_depth, 16)
  expect_equal(m$frames, 1000)
  expect_identical(m$wave_hash, sonicscrewdriver:::.waveHash(w, "full"))
})

test_that("calculateMetadata of part of a file hashes the whole file", {
  f <- extdata("AUDIOMOTH.WAV")
  w <- readAudio(f, from=1, to=2)
  m <- calculated(w)
  expect_identical(m$hash, digest::digest(file=f, algo="sha256"))
  expect_equal(m$bytes, file.size(f))
  expect_equal(m$frames, length(w@left))
})

test_that("calculateMetadata refuses a wave whose file has gone", {
  f <- withr::local_tempfile(fileext=".wav")
  file.copy(extdata("AUDIOMOTH.WAV"), f)
  w <- readAudio(f)
  unlink(f)
  expect_error(calculateMetadata(w), "no longer exists")
})

test_that("calculateMetadata refuses anything that is not a wave", {
  expect_error(calculateMetadata(1:10), "takes a Wave")
})

test_that("a process keeps the fields of the file as read", {
  w <- calculateMetadata(readAudio(extdata("AUDIOMOTH.WAV")))
  withr::local_options(sonicscrewdriver.provenance="full")
  fw <- filterWave(w, bandpass(1000, 4000))
  expect_identical(fw@metadata$calculatedMetadata, w@metadata$calculatedMetadata)
  expect_identical(fw@processing[[1]]$input_hash, w@metadata$calculatedMetadata$wave_hash)
})
