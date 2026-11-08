test_that("entropyStats analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(pmax(pmin(rnorm(n) * 15000, 32767), -32768)), samp.rate=sr, bit=16)

  # The channels must give different answers for the test to mean anything.
  expect_false(isTRUE(all.equal(entropyStats(L), entropyStats(R))))

  # entropyStats returns a list, so allChannels() does not wrap it again.
  expect_equal(entropyStats(tuneR::stereo(L, R)), list(entropyStats(L), entropyStats(R)))
  expect_equal(
    entropyStats(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)),
    list(entropyStats(L), entropyStats(R))
  )
})

test_that("entropyStats rejects things that are not waves", {
  expect_error(entropyStats("string"), "Expecting a Wave object")
})

test_that("a wave shorter than the spectrum's window is refused, naming both", {
  # meanspec() failed with "wrong sign in 'by' argument" from seq(), so a window
  # of windowing() shorter than 512 samples gave a message naming neither.
  w <- tuneR::Wave(round(10000*sin(2*pi*500*(0:8191)/8000)), samp.rate=8000, bit=16)
  expect_error(
    entropyStats(cutws(w, 1, 256)),
    "entropyStats() takes its spectrum in windows of 512 samples, and the wave holds 256.",
    fixed=TRUE
  )
  expect_error(
    windowing(w, window.length=256, FUN=function(x, ...) entropyStats(x)),
    "windows of 512 samples", fixed=TRUE
  )
  expect_type(entropyStats(cutws(w, 1, 512)), "list")
})

test_that("entropyStats asks meanspec not to plot", {
  # meanspec() plots by default, so asking for four numbers drew a spectrum as a
  # side effect: under Rscript that silently wrote Rplots.pdf, and inside
  # windowing() over a ten-minute recording it drew one plot per window.
  w <- tuneR::sine(440, duration=4096, samp.rate=44100)

  plot.arg <- NULL
  local_mocked_bindings(
    meanspec = function(wave, ...) {
      args <- list(...)
      plot.arg <<- if ("plot" %in% names(args)) args$plot else NA
      seewave::meanspec(wave, plot=FALSE)
    },
    .package = "sonicscrewdriver"
  )

  entropyStats(w)
  expect_false(is.na(plot.arg))
  expect_false(plot.arg)
})

test_that("a tagged wave gives the entropies of the wave it holds", {
  # seewave takes a wave only if its class is exactly "Wave", so meanspec() read
  # a TaggedWave as bare samples and failed with "argument "f" is missing", both
  # called directly and through tagApply(), which hands this function the tagged
  # wave.
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(pmax(pmin(rnorm(n) * 15000, 32767), -32768)), samp.rate=sr, bit=16)

  waves <- list(L, tuneR::stereo(L, R), tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16))
  for (w in waves) {
    tw <- tagWave(w)
    expect_equal(entropyStats(tw), entropyStats(w))

    r <- tagApply(tw, entropyStats)
    expect_length(attr(r, "processing"), 1)
    attr(r, "processing") <- NULL
    expect_equal(r, entropyStats(w))
  }
})
