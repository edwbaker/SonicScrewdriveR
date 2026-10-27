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

#The frequency holding the most energy, in Hz.
peakFrequency <- function(wave) {
  spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  spec[which.max(spec[, 2]), 1] * 1000
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
