test_that("equivalent sizes", {
  expect_equal(
    audio_filesize(duration=1, duration.unit = "minutes"),
    audio_filesize(duration=60, duration.unit = "seconds")
  )
  expect_equal(
    audio_filesize(duration=1, duration.unit = "hours"),
    audio_filesize(duration=3600, duration.unit = "seconds")
  )
  expect_equal(
    audio_filesize(duration=1, duration.unit = "days"),
    audio_filesize(duration=86400, duration.unit = "seconds")
  )
  expect_equal(
    44100*16,
    audio_filesize(samp.rate=44100, bit.depth=16, duration=1, duration.unit="seconds", output.unit="bits")
  )
  expect_equal(
    44100*16/8,
    audio_filesize(samp.rate=44100, bit.depth=16, duration=1, duration.unit="seconds", output.unit="bytes")
  )
  expect_equal(
    humanBytes(44100*16/8),
    audio_filesize(samp.rate=44100, bit.depth=16, duration=1, duration.unit="seconds", output.unit="human")
  )
  expect_equal(
    44100*16*2,
    audio_filesize(samp.rate=44100, bit.depth=16, channels=2, duration=1, duration.unit="seconds", output.unit="bits")
  )
})
