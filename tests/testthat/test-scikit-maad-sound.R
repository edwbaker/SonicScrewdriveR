test_that(".maadSpectrogramMode names the mode as scikit-maad does", {
  #Pure R, so these run whether or not scikit-maad is installed.
  expect_equal(.maadSpectrogramMode("power"), "psd")
  expect_equal(.maadSpectrogramMode("amplitude"), "amplitude")
  expect_equal(.maadSpectrogramMode("complex"), "complex")

  #scikit-maad answers an unknown mode with UnboundLocalError, so it is refused
  #here by name instead.
  expect_error(.maadSpectrogramMode("psd"), "mode must be one of")
  expect_error(.maadSpectrogramMode("dB"), "mode must be one of")
  expect_error(.maadSpectrogramMode(c("power", "amplitude")), "mode must be one of")
  expect_error(.maadSpectrogramMode(1), "mode must be one of")
})

test_that("maad_spectrogram computes the mode it was asked for", {
  skip_if_no_maad()
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)
  maad <- getMaad()

  power <- maad_spectrogram(w, maad=maad)
  expect_equal(power@mode, "power")
  expect_true(is.numeric(power@Sxx))

  amplitude <- maad_spectrogram(w, mode="amplitude", maad=maad)
  expect_equal(amplitude@mode, "amplitude")
  expect_true(is.numeric(amplitude@Sxx))
  #An amplitude spectrogram is the square root of the power one, so asking for
  #one and getting the other is what the equality here would hide.
  expect_equal(amplitude@Sxx^2, power@Sxx)
  expect_false(isTRUE(all.equal(amplitude@Sxx, power@Sxx)))

  complex <- maad_spectrogram(w, mode="complex", maad=maad)
  expect_equal(complex@mode, "complex")
  expect_true(is.complex(complex@Sxx))
  expect_equal(Mod(complex@Sxx)^2, power@Sxx)

  #The default stays the power spectrogram scikit-maad computes when given no
  #mode at all.
  expect_equal(
    power@Sxx,
    maad$sound$spectrogram(x=maad_wave(w), fs=w@samp.rate)[[1]]
  )

  expect_error(maad_spectrogram(w, mode="psd", maad=maad), "mode must be one of")
})

test_that("an index built on a non-power spectrogram agrees with the power one", {
  skip_if_no_maad()
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)
  maad <- getMaad()

  power <- maad_spectrogram(w, maad=maad)
  expected <- maad_acoustic_complexity_index(power, maad=maad)$ACI_sum
  for (mode in c("amplitude", "complex")) {
    spec <- maad_spectrogram(w, mode=mode, maad=maad)
    expect_equal(maad_acoustic_complexity_index(spec, maad=maad)$ACI_sum, expected)
  }
})
