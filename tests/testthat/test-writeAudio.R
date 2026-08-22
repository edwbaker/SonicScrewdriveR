test_that("writeAudio writes WAVE files", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))

  f <- tempfile(fileext=".wav")
  expect_equal(writeAudio(w, f), f)
  expect_true(file.exists(f))

  r <- readAudio(f)
  expect_equal(r@left, w@left)
  expect_equal(r@samp.rate, w@samp.rate)
  expect_equal(r@bit, w@bit)
  expect_equal(r@stereo, w@stereo)
  unlink(f)
})

test_that("writeAudio writes stereo and multichannel waves", {
  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"))
  f <- tempfile(fileext=".wav")
  writeAudio(w, f)
  r <- readAudio(f)
  expect_equal(r@left, w@left)
  expect_equal(r@right, w@right)
  unlink(f)

  d <- matrix(rep(1:100, 3), ncol=3)
  colnames(d) <- c("FL", "FR", "FC")
  mc <- tuneR::WaveMC(d, samp.rate=44100, bit=16)
  f <- tempfile(fileext=".wav")
  writeAudio(mc, f)
  r <- tuneR::readWave(f, toWaveMC=TRUE)
  expect_equal(ncol(r@.Data), 3)
  expect_equal(r@.Data[,1], d[,1])
  unlink(f)
})

test_that("writeAudio writes tagged waves as audio", {
  w <- tagWave(tuneR::sine(440, duration=1000))
  f <- tempfile(fileext=".wav")
  writeAudio(w, f)
  expect_true(file.exists(f))
  expect_false(is(readAudio(f), "TaggedWave"))
  unlink(f)
})

test_that("writeAudio uses the mime argument in preference to the extension", {
  w <- tuneR::sine(440, duration=1000)
  f <- tempfile(fileext=".dat")
  writeAudio(w, f, mime="audio/x-wav")
  expect_true(file.exists(f))
  expect_equal(length(readAudio(f, mime="audio/x-wav")@left), 1000)
  unlink(f)
})

test_that("writeAudio rejects what it cannot write", {
  w <- tuneR::sine(440, duration=1000)
  expect_error(writeAudio(1, tempfile(fileext=".wav")), "Expecting a Wave or WaveMC object")
  expect_error(writeAudio(w, c("a.wav", "b.wav")), "file must be a single filename")
  expect_error(writeAudio(w, tempfile(fileext=".txt")), "Not an audio mime type")
  expect_error(writeAudio(w, tempfile(fileext=".notanextension")), "Could not determine the format")
})

test_that("writeAudio writes compressed formats", {
  skip_if_not_installed("av")
  w <- tuneR::sine(440, duration=4410, samp.rate=44100)
  w <- suppressWarnings(tuneR::normalize(w, unit="16"))

  f <- tempfile(fileext=".flac")
  writeAudio(w, f)
  expect_true(file.exists(f))
  r <- readAudio(f)
  expect_equal(r@samp.rate, 44100)
  #FLAC is lossless, so the audio must survive the round trip unchanged.
  expect_equal(r@bit, w@bit)
  expect_equal(r@left, w@left)
  unlink(f)

  f <- tempfile(fileext=".mp3")
  writeAudio(w, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
  unlink(f)
})

test_that("writeAudio reports what an encoder refused", {
  skip_if_not_installed("av")
  #Opus only accepts 48kHz and its divisors.
  w <- suppressWarnings(tuneR::normalize(tuneR::sine(440, duration=4410, samp.rate=44100), unit="16"))
  expect_error(writeAudio(w, tempfile(fileext=".opus")), "Could not write")
})

test_that("FLAC can be written without the av package", {
  skip_if(Sys.which("flac") == "", "the flac program is not installed")
  w <- suppressWarnings(tuneR::normalize(tuneR::sine(440, duration=4410, samp.rate=44100), unit="16"))
  f <- tempfile(fileext=".flac")
  expect_equal(.writeAudioFlac(w, f), f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
  unlink(f)
})
