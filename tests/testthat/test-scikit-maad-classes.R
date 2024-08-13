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

test_that(".spectrogram_maad_dB works as expected", {
  powers_spec = new("spectrogram_maad", Sxx=matrix(3), mode="power")
  db_spec = .spectrogram_maad_dB(powers_spec)
  expect_equal(10 * log10(3), db_spec@Sxx[[1,1]])
  expect_equal("dB", db_spec@mode)
})
