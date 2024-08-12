test_that("maad_temporal_median works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_temporal_median(w)
  expect_true(is.numeric(ret))

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

  ret <- maad_temporal_entropy(w)
  expect_true(is.numeric(ret))

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

  ret <- maad_temporal_activity(w)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfrac", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

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

test_that("maad_temporal_events works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_temporal_events(w)
  expect_equal(length(ret), 4)
  expect_equal(names(ret), c("EVTfrac", "EVTcount", "EVTmean", "EVN"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
  expect_true(is.logical(ret[[4]]))

  maad <- getMaad()

  ret <- maad_temporal_events(w, maad=maad)
  expect_equal(length(ret), 4)
  expect_equal(names(ret), c("EVTfrac", "EVTcount", "EVTmean", "EVN"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
  expect_true(is.logical(ret[[4]]))

  ret <- maad_temporal_events(w, dB_threshold =4, maad=maad)
  expect_equal(length(ret), 4)
  expect_equal(names(ret), c("EVTfrac", "EVTcount", "EVTmean", "EVN"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
  expect_true(is.logical(ret[[4]]))

  ret <- maad_temporal_events(w, mode="hilbert", maad=maad)
  expect_equal(length(ret), 4)
  expect_equal(names(ret), c("EVTfrac", "EVTcount", "EVTmean", "EVN"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
  expect_true(is.logical(ret[[4]]))

  ret <- maad_temporal_events(w, Nt=256, maad=maad)
  expect_equal(length(ret), 4)
  expect_equal(names(ret), c("EVTfrac", "EVTcount", "EVTmean", "EVN"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
  expect_true(is.logical(ret[[4]]))
})

test_that("maad_acoustic_complexity_index works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_acoustic_complexity_index(w)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACI_xx", "ACI_per_bin", "ACI_sum"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  maad <- getMaad()

  ret <- maad_acoustic_complexity_index(w, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACI_xx", "ACI_per_bin", "ACI_sum"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  w <- maad_spectrogram(w)

  ret <- maad_acoustic_complexity_index(w)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACI_xx", "ACI_per_bin", "ACI_sum"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  ret <- maad_acoustic_complexity_index(w, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACI_xx", "ACI_per_bin", "ACI_sum"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
})

test_that("maad_frequency_entropy works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_frequency_entropy(w)
  expect_equal(length(ret), 2)
  expect_equal(names(ret), c("Hf", "Ht_per_bin"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))

  maad <- getMaad()

  ret <- maad_frequency_entropy(w, maad=maad)
  expect_equal(length(ret), 2)
  expect_equal(names(ret), c("Hf", "Ht_per_bin"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))

  ret <- maad_frequency_entropy(w, compatibility="seewave", maad=maad)
  expect_equal(length(ret), 2)
  expect_equal(names(ret), c("Hf", "Ht_per_bin"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))

  ret <- maad_frequency_entropy(tagWave(w), maad=maad)
  expect_equal(length(ret), 2)
  expect_equal(names(ret), c("Hf", "Ht_per_bin"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))

  ret <- maad_frequency_entropy(tuneR::WaveMC(tuneR::stereo(w,w)), maad=maad)
  expect_equal(length(ret), 2)
  expect_equal(names(ret), c("Hf", "Ht_per_bin"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
})

test_that("maad_number_of_peaks works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_number_of_peaks(w)
  expect_true(is.numeric(ret))

  maad <- getMaad()

  ret <- maad_number_of_peaks(w, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, mode="linear", maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, min_peak_val=400, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, min_freq_dist=400, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, slopes=c(2,2), maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, prominence=1, maad=maad)
  expect_true(is.numeric(ret))

  ret <- maad_number_of_peaks(w, prominence=c(0, 100), maad=maad)
  expect_true(is.numeric(ret))
})

test_that("maad_spectral_activity works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  ret <- maad_spectral_activity(w)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfract", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  maad <- getMaad()

  ret <- maad_spectral_activity(w, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfract", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))

  ret <- maad_spectral_activity(w, dB_threshold =4, maad=maad)
  expect_equal(length(ret), 3)
  expect_equal(names(ret), c("ACTfract", "ACTcount", "ACTmean"))
  expect_true(is.numeric(ret[[1]]))
  expect_true(is.numeric(ret[[2]]))
  expect_true(is.numeric(ret[[3]]))
})
