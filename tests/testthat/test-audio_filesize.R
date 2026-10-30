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

test_that("audio_filesize refuses a recording that could not have been made", {
  # samp.rate, bit.depth and channels were multiplied raw, so these returned
  # negative sizes, or zero, rather than refusing the arithmetic.
  expect_error(
    audio_filesize(samp.rate=-44100, bit.depth=16, channels=1, duration=1),
    "Sample rate must be greater than zero."
  )
  expect_error(
    audio_filesize(samp.rate=0, bit.depth=16, channels=1, duration=1),
    "Sample rate must be greater than zero."
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=0, channels=1, duration=1),
    "bit.depth must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=-16, channels=1, duration=1),
    "bit.depth must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16.5, channels=1, duration=1),
    "bit.depth must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16, channels=-2, duration=1),
    "channels must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16, channels=0, duration=1),
    "channels must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16, channels=1.5, duration=1),
    "channels must be a positive whole number"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16, channels=1, duration=-1),
    "Time in Seconds cannot be negative"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth="sixteen", channels=1, duration=1),
    "bit.depth must be numeric"
  )
  expect_error(
    audio_filesize(samp.rate=44100, bit.depth=16, channels="two", duration=1),
    "channels must be numeric"
  )
})

test_that("audio_filesize still sizes the recordings it sized", {
  expect_equal(
    audio_filesize(samp.rate=44100, bit.depth=16, channels=1, duration=1),
    44100*16
  )
  expect_equal(
    audio_filesize(samp.rate=96000, bit.depth=24, channels=2, duration=1, duration.unit="years"),
    96000*24*2*60*60*24*365
  )
  expect_equal(audio_filesize(duration=0), 0)
})
