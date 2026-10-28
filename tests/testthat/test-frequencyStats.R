test_that("inputs are correct", {
  expect_error(frequencyStats("string", output.unit="Hz"), "Expecting a Wave object")
  expect_error(frequencyStats(1, output.unit="Hz"), "Expecting a Wave object")
})

test_that("plotting is ok", {
  data(sheep, package="seewave")
  expect_silent(frequencyStats(sheep, plot=TRUE, output.unit="Hz"))
  expect_silent(frequencyStats(sheep, plot=TRUE, output.unit="kHz"))
})

test_that("frequencyStats analyses each channel of multi-channel input", {
  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)

  fs <- function(w) frequencyStats(w, output.unit="Hz")
  expect_false(isTRUE(all.equal(fs(L), fs(R))))

  # frequencyStats returns a list, so allChannels() does not wrap it again.
  expect_equal(fs(tuneR::stereo(L, R)), list(fs(L), fs(R)))
  expect_equal(
    fs(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)),
    list(fs(L), fs(R))
  )
})

test_that("frequencyStats passes arguments to each channel", {
  sr <- 8000
  n <- 8000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, L)

  # warn=FALSE throughout: a pure tone leaves a single bin above the threshold
  # once lowcut is applied, which frequencyStats() rightly warns about.
  expect_equal(
    frequencyStats(st, lowcut=2000, warn=FALSE, output.unit="Hz"),
    list(
      frequencyStats(L, lowcut=2000, warn=FALSE, output.unit="Hz"),
      frequencyStats(L, lowcut=2000, warn=FALSE, output.unit="Hz")
    )
  )
  expect_equal(
    frequencyStats(st, warn=FALSE, output.unit="kHz"),
    list(frequencyStats(L, warn=FALSE, output.unit="kHz"), frequencyStats(L, warn=FALSE, output.unit="kHz"))
  )
})

# A spectrum whose power is at or above half its maximum in exactly bins 5 to 7,
# which are 4, 5 and 6 kHz. frequencyStats() squares the amplitudes it is given.
# The frequencies are in kHz, as a spectrum from seewave has them.
knownSpectrum <- function(power) {
  return(cbind(seq(0, 10, length.out=11), sqrt(power)))
}
knownWave <- tuneR::sine(440, duration=4410, samp.rate=44100)

test_that("frequencyStats reads the edges of the threshold region", {
  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz")$`-3dB`
  # The lower edge used to be read one bin below the region, making every
  # bandwidth one bin too wide and every centre half a bin too low.
  expect_equal(r$min, 4000)
  expect_equal(r$max, 6000)
  expect_equal(r$bandwidth, 2000)
  expect_equal(r$centre, 5000)
  expect_equal(r$peak, 5000)
})

test_that("frequencyStats returns one value per statistic when maxima tie", {
  # which(y == max(y)) returned every tied bin.
  spec <- knownSpectrum(c(0,0,0,0,4,4,4,0,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz")
  for (threshold in c("-3dB", "-10dB")) {
    for (stat in c("peak", "min", "max", "centre", "bandwidth", "Q")) {
      expect_length(r[[threshold]][[stat]], 1)
    }
  }
})

test_that("frequencyStats takes its low cut in Hz", {
  # The spectrum has its frequency axis in kHz, and this argument used to be in
  # kHz with it, so a cut of 4000 zeroed the whole spectrum rather than the bins
  # below 4kHz.
  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, lowcut=4000, warn=FALSE, output.unit="Hz")$`-3dB`
  expect_equal(r$min, 4000)
  expect_equal(r$max, 6000)

  # A cut above everything in the spectrum still leaves nothing to measure.
  expect_error(
    frequencyStats(knownWave, wave_spec=spec, lowcut=7000, warn=FALSE, output.unit="Hz"),
    "No signal above lowcut"
  )
})

test_that("frequencyStats says so when there is no signal", {
  # The outer statistics were taken over an empty set, giving an infinite
  # frequency and an error about the sample rate.
  spec <- knownSpectrum(rep(0, 11))
  expect_error(
    frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz"),
    "No signal above lowcut"
  )
})

test_that("frequencyStats separates the two thresholds", {
  # Power at half maximum over bins 5-7, and at a tenth over bins 4-8.
  spec <- knownSpectrum(c(0,0,0,0.5,2,4,2,0.5,0,0,0))
  r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz")
  expect_equal(r$`-3dB`$min, 4000)
  expect_equal(r$`-3dB`$max, 6000)
  expect_equal(r$`-10dB`$min, 3000)
  expect_equal(r$`-10dB`$max, 7000)
  expect_gt(r$`-10dB`$bandwidth, r$`-3dB`$bandwidth)
})

test_that("frequencyStats returns its frequencies in the unit asked for", {
  spec <- knownSpectrum(c(0,0,0,0.5,2,4,2,0.5,0,0,0))
  hz <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz")
  khz <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="kHz")

  frequencies <- c(
    "min", "max", "longest_min", "longest_max", "peak", "centre", "bandwidth",
    "outer min", "outer max", "outer bandwidth"
  )
  for (threshold in c("-3dB", "-10dB")) {
    expect_named(hz[[threshold]], c(frequencies[1:7], "Q", frequencies[8:10]))
    for (stat in frequencies) {
      expect_equal(khz[[threshold]][[stat]], hz[[threshold]][[stat]] / 1000, info=paste(threshold, stat))
    }
    # Q is a ratio of two frequencies, and the same in either unit.
    expect_equal(khz[[threshold]]$Q, hz[[threshold]]$Q)
  }
  expect_equal(khz$`spectral flatness`, hz$`spectral flatness`)

  # kHz, the unit of the spectrum, gives back the spectrum's own frequencies.
  expect_equal(khz$`-3dB`$min, 4)
  expect_equal(khz$`-10dB`$max, 7)
})

test_that("frequencyStats refuses a unit it does not return", {
  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))
  fs <- function(unit) frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit=unit)
  expect_error(fs("MHz"), "Unknown output.unit for frequencyStats: MHz")
  expect_error(fs("hz"), "Unknown output.unit for frequencyStats: hz")
  expect_error(fs(NA_character_), "Unknown output.unit for frequencyStats: NA")
  # Each of these passes a check of its values one at a time. A factor is looked
  # up in the table of units by its position, so the last of them would return
  # kHz labelled as Hz.
  expect_error(fs(c("Hz", "kHz")), "output.unit must be a single unit")
  expect_error(fs(character(0)), "output.unit must be a single unit")
  expect_error(fs(NULL), "output.unit must be a single unit")
  expect_error(fs(NA), "output.unit must be a single unit")
  expect_error(fs(1000), "output.unit must be a single unit")
  expect_error(fs(factor("kHz")), "output.unit must be a single unit")
  expect_error(fs(factor("Hz", levels=c("kHz", "Hz"))), "output.unit must be a single unit")
})

test_that("frequencyStats warns once per session when its unit is left unset", {
  state <- sonicscrewdriver:::.frequencyStatsState
  warned <- state$warned
  withr::defer(state$warned <- warned)
  state$warned <- NULL

  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))
  expect_warning(
    r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE),
    "will return them in Hz by default in a future release"
  )
  # The default is still kHz.
  expect_equal(r, frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="kHz"))
  expect_no_warning(frequencyStats(knownWave, wave_spec=spec, warn=FALSE))
})

test_that("frequencyStats does not warn of its unit when one is given", {
  state <- sonicscrewdriver:::.frequencyStatsState
  warned <- state$warned
  withr::defer(state$warned <- warned)
  spec <- knownSpectrum(c(0,0,0,0,2,4,2,0,0,0,0))

  # However the argument arrives: in full, abbreviated, by position, through a
  # wrapper's dots, or by do.call().
  calls <- list(
    function() frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="kHz"),
    function() frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output="kHz"),
    function() frequencyStats(knownWave, spec, FALSE, 1000, FALSE, "kHz"),
    function() (function(...) frequencyStats(knownWave, wave_spec=spec, warn=FALSE, ...))(output.unit="kHz"),
    function() do.call(frequencyStats, list(knownWave, wave_spec=spec, warn=FALSE, output.unit="kHz"))
  )
  expected <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="kHz")
  for (i in seq_along(calls)) {
    state$warned <- NULL
    expect_no_warning(r <- calls[[i]]())
    expect_equal(r, expected, info=i)
  }

  # A wrapper that forwards no unit leaves it unset.
  state$warned <- NULL
  expect_warning(
    (function(...) frequencyStats(knownWave, wave_spec=spec, warn=FALSE, ...))(),
    "will return them in Hz by default"
  )
})

test_that("frequencyStats warns of its unit once for multi-channel input", {
  state <- sonicscrewdriver:::.frequencyStatsState
  warned <- state$warned
  withr::defer(state$warned <- warned)
  state$warned <- NULL

  sr <- 8000
  n <- 8000
  set.seed(4)
  L <- tuneR::Wave(round(rnorm(n) * 15000), samp.rate=sr, bit=16)
  warnings <- 0
  r <- withCallingHandlers(
    frequencyStats(tuneR::stereo(L, L), warn=FALSE),
    warning = function(w) {
      warnings <<- warnings + 1
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(warnings, 1)
  expect_equal(r, rep(list(frequencyStats(L, warn=FALSE, output.unit="kHz")), 2))
})

test_that("frequencyStats checks its results against the sample rate in one unit", {
  # The results were checked against half the sample rate in kHz against Hz, so
  # a spectrum reaching 10kHz passed with a wave sampled at 8kHz, whose recording
  # ends at 4kHz.
  spec <- knownSpectrum(c(0,0,0,0,0,0,0,0,2,4,2))
  low <- tuneR::sine(440, duration=800, samp.rate=8000)
  expect_error(
    frequencyStats(low, wave_spec=spec, warn=FALSE, output.unit="Hz"),
    "Frequency is greater than half sample rate."
  )
  expect_no_error(frequencyStats(knownWave, wave_spec=spec, warn=FALSE, output.unit="Hz"))
})

test_that("frequencyStats plots its frequencies in the unit asked for", {
  drawn <- list()
  local_mocked_bindings(
    plot = function(x, ...) drawn$x <<- x,
    abline = function(..., v=NULL) drawn$v <<- c(drawn$v, v)
  )
  spec <- knownSpectrum(c(0,0,0,0.5,2,4,2,0.5,0,0,0))
  for (unit in c("Hz", "kHz")) {
    drawn <- list()
    r <- frequencyStats(knownWave, wave_spec=spec, warn=FALSE, plot=TRUE, output.unit=unit)
    scale <- if (unit == "Hz") 1000 else 1
    expect_equal(drawn$x, spec[, 1] * scale, info=unit)
    expect_true(all(drawn$v %in% unlist(r[c("-3dB", "-10dB")])), info=unit)
  }

  labels <- NULL
  local_mocked_bindings(plot = function(x, y, ..., xlab) labels <<- xlab)
  frequencyStats(knownWave, wave_spec=spec, warn=FALSE, plot=TRUE, output.unit="Hz")
  expect_equal(labels, "Frequency (Hz)")
  frequencyStats(knownWave, wave_spec=spec, warn=FALSE, plot=TRUE, output.unit="kHz")
  expect_equal(labels, "Frequency (kHz)")
})

test_that("sheepFrequencyStats is what frequencyStats() returns for sheep, in Hz", {
  # It was stored before the fixes to how the edges of a region are read, so it
  # held a -3dB bandwidth half as wide again as the one the function measures.
  data(sheep, package="seewave", envir=environment())
  expect_equal(sonicscrewdriver::sheepFrequencyStats, frequencyStats(sheep, output.unit="Hz"))
})
