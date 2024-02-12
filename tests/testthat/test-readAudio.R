test_that("Reading files works", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_equal(length(w@left), 192000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), mime="audio/x-wav")
  expect_equal(length(w@left), 192000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)

  # TODO: empty.wav
})
