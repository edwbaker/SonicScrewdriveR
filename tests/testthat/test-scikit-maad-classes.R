test_that(".spectrogram_maad_power works as expected", {
  skip_if_no_maad()
  f <- f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- readWave(f)

  spec <- .spectrogram_maad_power(w)
  expect_s4_class(spec, "spectrogram_maad")

  spec <- .spectrogram_maad_power(tagWave(w))
  expect_s4_class(spec, "spectrogram_maad")

  #Only the first channel of a WaveMC is analysed, and the caller is told.
  w <- tuneR::WaveMC(tuneR::stereo(w,w))
  expect_warning(spec <- .spectrogram_maad_power(w), "only the first of the 2 channels")
  expect_s4_class(spec, "spectrogram_maad")

  expect_warning(spec <- .spectrogram_maad_power(maad_spectrogram(w)), "only the first")
  expect_s4_class(spec, "spectrogram_maad")
})

test_that(".spectrogram_maad_dB works as expected", {
  powers_spec = new("spectrogram_maad", Sxx=matrix(3), mode="power")
  db_spec = .spectrogram_maad_dB(powers_spec)
  expect_equal(10 * log10(3), db_spec@Sxx[[1,1]])
  expect_equal("dB", db_spec@mode)
})

test_that(".spectrogram_maad_power converts each mode back to power", {
  #Pure R, so these run whether or not scikit-maad is installed.
  ext <- list(tmin=0, tmax=1, fmin=0, fmax=1)
  power <- matrix(c(1, 10, 100, 1000), 2, 2)
  spec <- function(Sxx, mode) {
    new("spectrogram_maad", Sxx=Sxx, tn=c(0, 1), fn=c(0, 1), extents=ext, mode=mode)
  }

  unchanged <- .spectrogram_maad_power(spec(power, "power"))
  expect_equal(unchanged@Sxx, power)
  expect_equal(unchanged@mode, "power")

  converted <- .spectrogram_maad_power(spec(sqrt(power), "amplitude"))
  expect_equal(converted@Sxx, power)
  expect_equal(converted@mode, "power")

  #A complex spectrogram carries the same magnitudes with a phase.
  phase <- complex(real=cos(1), imaginary=sin(1))
  converted <- .spectrogram_maad_power(spec(sqrt(power)*phase, "complex"))
  expect_equal(converted@Sxx, power)
  expect_equal(converted@mode, "power")

  converted <- .spectrogram_maad_power(spec(10*log10(power), "dB"))
  expect_equal(converted@Sxx, power)
  expect_equal(converted@mode, "power")

  expect_error(
    .spectrogram_maad_power(spec(power, "nonsense")),
    "cannot be converted to power"
  )
})

test_that(".spectrogram_maad_dB converts once and only once", {
  power <- matrix(c(1, 10, 100, 1000), 2, 2)
  once <- .spectrogram_maad_dB(new("spectrogram_maad", Sxx=power, mode="power"))
  expect_equal(once@Sxx, 10*log10(power))
  expect_equal(once@mode, "dB")

  #The second pass has to be a no-op rather than a second conversion.
  expect_equal(.spectrogram_maad_dB(once)@Sxx, once@Sxx)

  #A realistic dB spectrogram holds zero and negative values, so converting it
  #again is not merely wrong but not finite.
  real <- new("spectrogram_maad", Sxx=matrix(c(0, -3, 1, 2), 2, 2), mode="dB")
  twice <- .spectrogram_maad_dB(real)
  expect_equal(twice@Sxx, real@Sxx)
  expect_true(all(is.finite(twice@Sxx)))

  #An amplitude spectrogram wants 20*log10, which is 10*log10 of its square.
  amplitude <- new("spectrogram_maad", Sxx=matrix(c(1, 10), 1, 2), mode="amplitude")
  expect_equal(.spectrogram_maad_dB(amplitude)@Sxx, matrix(c(0, 20), 1, 2))
})
