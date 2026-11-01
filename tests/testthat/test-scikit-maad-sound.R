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

test_that(".maadSamples normalises as maad.sound.load() does", {
  #Pure R, so these run whether or not scikit-maad is installed.
  expect_null(.maadSamples("not a wave"))

  #Signed PCM is divided by half its range.
  signed <- c(-32768, 0, 16384)
  w <- tuneR::Wave(left=signed, samp.rate=44100, bit=16, pcm=TRUE)
  expect_equal(.maadSamples(w), signed/32768 - mean(signed/32768))

  #Eight bit PCM is unsigned, sits on 128 rather than on zero, and is divided
  #by the whole of its range.
  unsigned <- c(0, 128, 255)
  w8 <- tuneR::Wave(left=unsigned, samp.rate=44100, bit=8, pcm=TRUE)
  samples <- .maadSamples(w8)
  expect_equal(samples, unsigned/256 - mean(unsigned/256))
  expect_lt(abs(mean(samples)), 1e-12)
  expect_true(all(abs(samples) <= 1))

  #Floating point samples are in range already, so they are only centred.
  float <- c(-0.5, 0, 0.25)
  wf <- tuneR::Wave(left=float, samp.rate=44100, bit=32, pcm=FALSE)
  expect_equal(.maadSamples(wf), float - mean(float))

  #A WaveMC is read from its first column, as a Wave is from its left channel.
  mc <- tuneR::WaveMC(data=cbind(signed, rep(0, 3)), samp.rate=44100, bit=16)
  expect_equal(.maadSamples(mc), .maadSamples(w))
})

test_that(".maadSamples puts the bit depths of one recording on one scale", {
  sig <- sin(2*pi*seq(0, 1, length.out=4410)*440)*0.8
  w16 <- tuneR::Wave(left=round(sig*32768), samp.rate=4410, bit=16, pcm=TRUE)
  wf <- tuneR::Wave(left=sig, samp.rate=4410, bit=32, pcm=FALSE)
  #Untouched these differed by a factor of 32768, so every scale-dependent
  #index was a function of the container as much as of the recording.
  expect_equal(.maadSamples(w16), .maadSamples(wf), tolerance=1e-4)
  expect_true(all(abs(.maadSamples(w16)) <= 1))
})

test_that("maad_wave hands Python what scikit-maad's own reader would", {
  skip_if_no_maad()
  maad <- getMaad()
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  theirs <- as.numeric(maad$sound$load(f)[[1]])
  ours <- as.numeric(reticulate::py_to_r(maad_wave(readWave(f))))
  expect_equal(ours, theirs)
})

test_that("an index no longer depends on the bit depth it was stored at", {
  skip_if_no_maad()
  maad <- getMaad()
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)
  sig <- (w@left - mean(w@left))/32768
  w16 <- tuneR::Wave(left=round(sig*32768), samp.rate=w@samp.rate, bit=16, pcm=TRUE)
  wf <- tuneR::Wave(left=sig, samp.rate=w@samp.rate, bit=32, pcm=FALSE)
  #Before this these were 287.5 and 0.0088.
  expect_equal(
    maad_temporal_median(w16, maad=maad),
    maad_temporal_median(wf, maad=maad),
    tolerance=1e-4
  )

})
