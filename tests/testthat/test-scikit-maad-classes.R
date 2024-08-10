test_that(".spectrogram_maad_power works as expected", {
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  spec <- .spectrogram_maad_power(w)
  expect_s4_class(spec, "spectrogram_maad")

  spec <- .spectrogram_maad_power(tagWave(w))
  expect_s4_class(spec, "spectrogram_maad")

  w <- tuneR::WaveMC(tuneR::stereo(w,w))
  spec <- .spectrogram_maad_power(w)
  expect_s4_class(spec, "spectrogram_maad")

  spec <- .spectrogram_maad_power(maad_spectrogram(w))
  expect_s4_class(spec, "spectrogram_maad")
})
