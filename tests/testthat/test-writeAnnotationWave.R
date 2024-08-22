test_that("writeAnnotationWave works", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  a <- annotation(f, start = 0, end = 1)

  writeAnnotationWave(a)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  unlink("AUDIOMOTH_0-1.wav")

  b <- annotation(f, start = 1, end = 3)

  l <- list(a, b)

  writeAnnotationWave(l)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  expect_true(file.exists("AUDIOMOTH_1-3.wav"))
  unlink("AUDIOMOTH_0-1.wav")
  unlink("AUDIOMOTH_1-3.wav")


  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  wave <- readWave(f)
  a <- annotation(f, start = 0, end = 1)

  writeAnnotationWave(a, wave)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  unlink("AUDIOMOTH_0-1.wav")

  b <- annotation(f, start = 1, end = 3)

  l <- list(a, b)

  writeAnnotationWave(l, wave)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  expect_true(file.exists("AUDIOMOTH_1-3.wav"))
  unlink("AUDIOMOTH_0-1.wav")
  unlink("AUDIOMOTH_1-3.wav")
})
