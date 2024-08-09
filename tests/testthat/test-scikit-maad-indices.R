test_that("maad_temporal_median works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  maad <- getMaad()

  ret <- maad_temporal_median(w, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_temporal_median(w, mode="hilbert", maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_temporal_median(w, Nt=256, maad=maad)
  expect_true(is.numeric(ret))
})

test_that("maad_temporal_entropy works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  maad <- getMaad()

  ret <- maad_temporal_entropy(w, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_temporal_entropy(w, compatibility="seewave", maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_temporal_entropy(w, mode="hilbert", maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_temporal_entropy(w, Nt=256, maad=maad)
  expect_true(is.numeric(ret))
})

test_that("maad_temporal_activity works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  maad <- getMaad()

  ret <- maad_temporal_activity(w, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfrac", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  ret <- maad_temporal_activity(w, dB_threshold =4, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfrac", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  ret <- maad_temporal_activity(w, mode="hilbert", maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfrac", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  ret <- maad_temporal_activity(w, Nt=256, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfrac", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
})
