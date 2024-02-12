test_that("Reading files works", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), mime="audio/x-wav")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"))
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), mime="audio/mpeg")
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  # TODO: empty.wav

  # TODO: reading sections of file
})

test_that("readAudio rejects bad files", {
  expect_error(readAudio(system.file("extdata/CONFIG.TXT", package="sonicscrewdriver")), "Could not determine number of channels.")
})
