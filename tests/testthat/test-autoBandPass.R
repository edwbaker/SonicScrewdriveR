#Band-limited noise rather than a tone: a tone occupies a single spectrum bin, so
#it has no measurable bandwidth for the filter to be built from. The strong band
#is the one the filter should choose, the weak band the one it should discard.
bandWave <- function(samp.rate=8000, seconds=1, strong=c(1800, 2200), weak=c(3200, 3600)) {
  set.seed(42)
  noise <- tuneR::Wave(round(10000*rnorm(samp.rate*seconds)), samp.rate=samp.rate, bit=16)
  loud <- seewave::ffilter(noise, from=strong[1], to=strong[2], output="Wave")
  quiet <- seewave::ffilter(noise, from=weak[1], to=weak[2], output="Wave")
  tuneR::Wave(round(loud@left + 0.1*quiet@left), samp.rate=samp.rate, bit=16)
}

#A pure tone, which falls in a single bin of the spectrum and so has no
#bandwidth to measure at the -3dB threshold.
toneWave <- function(samp.rate=8000, seconds=1, frequency=2000) {
  times <- seq_len(samp.rate*seconds)/samp.rate
  tuneR::Wave(round(10000*sin(2*pi*frequency*times)), samp.rate=samp.rate, bit=16)
}

#The frequency holding the most energy, in Hz.
peakFrequency <- function(wave) {
  spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  unname(spec[which.max(spec[, 2]), 1]) * 1000
}

#Energy in a band, in kHz.
bandEnergy <- function(wave, from, to) {
  spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  sum(spec[spec[, 1] >= from & spec[, 1] <= to, 2])
}

test_that("something that is not a wave is refused", {
  expect_error(autoBandPass("sheep"), "Expecting a Wave object")
  expect_error(autoBandPass(1:100), "Expecting a Wave object")
})

test_that("a bandwidth the function does not measure is refused by name", {
  expect_error(autoBandPass(bandWave(), bw="-6dB"), "Unknown bw for autoBandPass: -6dB")
})

test_that("a number of bandwidths that is not whole is refused", {
  wave <- bandWave()
  expect_error(autoBandPass(wave, n.bw=1.5), "n.bw must be an integer.")
  expect_error(autoBandPass(wave, n.bw="two"), "n.bw must be an integer.")
  expect_error(autoBandPass(wave, n.bw=NA), "n.bw must be an integer.")
  expect_error(autoBandPass(wave, n.bw=c(1, 2)), "n.bw must be an integer.")
})

test_that("fewer than one bandwidth either side is refused", {
  #A band of no width, or one whose edges are the wrong way round, reached
  #seewave, which reported it as an error about its own arguments.
  wave <- bandWave()
  expect_error(autoBandPass(wave, n.bw=0), "n.bw must be at least 1")
  expect_error(autoBandPass(wave, n.bw=-1), "n.bw must be at least 1")
})

test_that("a low cut that is not one frequency the recording holds is refused", {
  #bandWave() is sampled at 8kHz, so its recording ends at 4kHz.
  wave <- bandWave()
  expect_error(autoBandPass(wave, lowcut=c(0, 1000)), "lowcut must be a single frequency")
  expect_error(autoBandPass(wave, lowcut=NA), "lowcut must be a single frequency")
  expect_error(autoBandPass(wave, lowcut="1000"), "Frequency must be numeric")
  expect_error(autoBandPass(wave, lowcut=-100), "Frequency must be positive")
  #At or above half the sample rate the high-pass leaves nothing to measure, and
  #seewave reported a band starting there as an error about its own arguments.
  expect_error(autoBandPass(wave, lowcut=4000), "lowcut must be below half the sample rate")
  expect_error(autoBandPass(wave, lowcut=9000), "lowcut must be below half the sample rate")
})

test_that("a whole number written as a double is accepted", {
  #1 rather than 1L is a double, which is also the default of the function.
  wave <- bandWave()
  expect_no_error(autoBandPass(wave, n.bw=1))
  expect_no_error(autoBandPass(wave, n.bw=2L))
})

test_that("a filtered wave of the same shape is returned", {
  wave <- bandWave()
  filtered <- autoBandPass(wave)
  expect_s4_class(filtered, "Wave")
  expect_equal(filtered@samp.rate, wave@samp.rate)
  expect_equal(length(filtered@left), length(wave@left))
  expect_equal(filtered@bit, wave@bit)
})

test_that("the band kept is the one around the strongest frequency", {
  wave <- bandWave()
  filtered <- autoBandPass(wave)
  expect_gt(peakFrequency(filtered), 1800)
  expect_lt(peakFrequency(filtered), 2200)
})

test_that("frequencies outside the band are reduced", {
  wave <- bandWave()
  filtered <- autoBandPass(wave)
  expect_lt(bandEnergy(filtered, 3.2, 3.6), bandEnergy(wave, 3.2, 3.6)/10)
  #The band of interest is left standing while the band away from it goes, so
  #the one is louder than the other by more than it was to begin with.
  before <- bandEnergy(wave, 1.8, 2.2) / bandEnergy(wave, 3.2, 3.6)
  after <- bandEnergy(filtered, 1.8, 2.2) / bandEnergy(filtered, 3.2, 3.6)
  expect_gt(after, before)
})

test_that("more bandwidths either side keep more of the recording", {
  wave <- bandWave()
  narrow <- autoBandPass(wave, n.bw=1)
  wide <- autoBandPass(wave, n.bw=3)
  expect_gt(sum(abs(wide@left)), sum(abs(narrow@left)))
})

test_that("the two bandwidths measured give different bands", {
  wave <- bandWave()
  #-10dB is measured across a wider part of the peak than -3dB, so it keeps more.
  narrow <- autoBandPass(wave, bw="-3dB")
  wide <- autoBandPass(wave, bw="-10dB")
  expect_gt(sum(abs(wide@left)), sum(abs(narrow@left)))
})

test_that("the low cut is applied before the centre frequency is measured", {
  #The stronger band is the lower one, so which band is chosen says whether the
  #low cut was applied before the centre frequency was measured.
  wave <- bandWave(strong=c(1100, 1400), weak=c(2400, 2700))

  low <- peakFrequency(autoBandPass(wave, lowcut=1000))
  expect_gt(low, 1100)
  expect_lt(low, 1400)

  #With the cut above the stronger band it is gone before the measurement, so
  #the weaker band is all that is left to choose.
  high <- peakFrequency(autoBandPass(wave, lowcut=2000))
  expect_gt(high, 2400)
  expect_lt(high, 2700)
})

test_that("a band below 1kHz is chosen when the low cut allows it", {
  #frequencyStats() takes a low cut of its own, which defaults to 1kHz. That
  #default was left standing whatever was asked for here, so everything below
  #1kHz was zeroed before the centre frequency was measured and no band below
  #1kHz could be chosen, however low the high-pass was set.
  wave <- bandWave(strong=c(100, 400), weak=c(2400, 2700))

  low <- peakFrequency(autoBandPass(wave, lowcut=0))
  expect_gt(low, 100)
  expect_lt(low, 400)

  #The floor still holds where one is asked for: the stronger band is below the
  #default cut, so it is not the band the filter is built from.
  high <- peakFrequency(autoBandPass(wave))
  expect_gt(high, 2400)
  expect_lt(high, 2700)
})

test_that("a band reaching past the ends of the recording is kept as far as the recording goes", {
  #A bandwidth either side of a low centre frequency puts the lower edge of the
  #band below zero, which seewave reported as an error about subscripts.
  low <- bandWave(strong=c(100, 400), weak=c(2400, 2700))
  filtered <- autoBandPass(low, bw="-10dB", lowcut=0)
  expect_s4_class(filtered, "Wave")
  expect_gt(peakFrequency(filtered), 100)
  expect_lt(peakFrequency(filtered), 400)

  #And at the other end the upper edge above the Nyquist frequency.
  high <- bandWave(strong=c(3500, 3900), weak=c(1200, 1500))
  filtered <- autoBandPass(high, bw="-10dB", n.bw=3)
  expect_s4_class(filtered, "Wave")
  expect_gt(peakFrequency(filtered), 3500)
  expect_lt(peakFrequency(filtered), 3900)
})

test_that("a pure tone is filtered rather than failing in seewave", {
  #A tone falls in a single bin of the spectrum, so its bandwidth is zero and
  #the filter ran from the centre to the centre. What the caller saw was
  #"'from' should be strictly inferior to 'to'" from seewave, which named
  #neither this function nor the reason.
  tone <- toneWave()
  expect_warning(
    filtered <- autoBandPass(tone),
    "-3dB: the signal has no measurable bandwidth"
  )
  expect_s4_class(filtered, "Wave")
  expect_equal(peakFrequency(filtered), 2000)
  #One bin of the spectrum is kept, so what is left is the tone.
  expect_gt(bandEnergy(filtered, 1.9, 2.1), 0.9*bandEnergy(filtered, 0, 4))
})

test_that("a tone below the default low cut is filtered around the tone", {
  #Both the low cut and the bandwidth are needed here: the band is measured
  #where the tone is, and is a single bin wide once it is found.
  tone <- toneWave(frequency=300)
  expect_warning(filtered <- autoBandPass(tone, lowcut=0), "no measurable bandwidth")
  expect_lt(abs(peakFrequency(filtered) - 300), 20)
  expect_gt(bandEnergy(filtered, 0.2, 0.4), 0.9*bandEnergy(filtered, 0, 4))
})

test_that("a threshold that does measure a bandwidth is not widened", {
  #The -10dB region of the same tone is wider than a single bin, so nothing is
  #imposed on it. Its -3dB region is a single bin, which frequencyStats() warns
  #about, but that is not the threshold this filter was asked for.
  tone <- toneWave()
  expect_no_warning(filtered <- autoBandPass(tone, bw="-10dB"))
  expect_equal(peakFrequency(filtered), 2000)
})

test_that("the band is built from frequencies in Hz without a warning about their unit", {
  #frequencyStats() warns when its unit is left to the default, which is
  #changing. This function asks for Hz, the unit of the filter it builds, so
  #nothing it does is affected by that change or warns of it.
  state <- sonicscrewdriver:::.frequencyStatsState
  warned <- state$warned
  withr::defer(state$warned <- warned)
  state$warned <- NULL

  wave <- bandWave()
  expect_no_warning(filtered <- autoBandPass(wave))
  expect_gt(peakFrequency(filtered), 1800)
  expect_lt(peakFrequency(filtered), 2200)
  expect_null(state$warned)
})
