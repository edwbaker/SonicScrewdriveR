#Band-limited noise rather than a tone: a tone occupies a single spectrum bin, so
#it has no measurable bandwidth for the filter to be built from. The strong band
#is the one the filter should choose, the weak band the one it should discard.
#seewave's ffilter() returns what it passes at one part in its window length of
#the level it had, so the bands are scaled back by that length. Left at a few
#units, a band filtered again and rounded to 16 bit came back as mostly zeros.
bandWave <- function(samp.rate=8000, seconds=1, strong=c(1800, 2200), weak=c(3200, 3600)) {
  set.seed(42)
  noise <- tuneR::Wave(round(10000*rnorm(samp.rate*seconds)), samp.rate=samp.rate, bit=16)
  loud <- seewave::ffilter(noise, from=strong[1], to=strong[2], output="Wave")
  quiet <- seewave::ffilter(noise, from=weak[1], to=weak[2], output="Wave")
  tuneR::Wave(round(1024*(loud@left + 0.1*quiet@left)), samp.rate=samp.rate, bit=16)
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
  # A WaveMC is now accepted, as the filter underneath can take one, so the
  # refusal names both of the classes that are.
  expect_error(autoBandPass("sheep"), "Expecting a Wave or WaveMC object")
  expect_error(autoBandPass(1:100), "Expecting a Wave or WaveMC object")
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
  # Both are normalised to the peak of the input, so what each keeps is measured
  # by how closely it follows the input rather than by its level.
  expect_gt(stats::cor(wide@left, wave@left), stats::cor(narrow@left, wave@left))
})

test_that("the two bandwidths measured give different bands", {
  wave <- bandWave()
  #-10dB is measured across a wider part of the peak than -3dB, so it keeps more.
  narrow <- autoBandPass(wave, bw="-3dB")
  wide <- autoBandPass(wave, bw="-10dB")
  expect_gt(stats::cor(wide@left, wave@left), stats::cor(narrow@left, wave@left))
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

#A second band-limited channel, so the two channels of a stereo wave hold
#different signals and must choose different bands.
otherBandWave <- function(samp.rate=8000, seconds=1, strong=c(3200, 3600)) {
  set.seed(7)
  noise <- tuneR::Wave(round(10000*rnorm(samp.rate*seconds)), samp.rate=samp.rate, bit=16)
  loud <- seewave::ffilter(noise, from=strong[1], to=strong[2], output="Wave")
  tuneR::Wave(round(1024*loud@left), samp.rate=samp.rate, bit=16)
}

test_that("a stereo wave keeps both of its channels", {
  # seewave reads the left slot of a Wave, so a stereo wave came back mono with
  # its right channel silently discarded.
  L <- bandWave()
  R <- otherBandWave()
  st <- tuneR::stereo(L, R)
  filtered <- autoBandPass(st)

  expect_s4_class(filtered, "Wave")
  expect_true(filtered@stereo)
  expect_equal(length(filtered@left), length(st@left))
  expect_equal(length(filtered@right), length(st@right))
  expect_equal(filtered@samp.rate, st@samp.rate)
  expect_equal(filtered@bit, st@bit)
  expect_equal(filtered@pcm, st@pcm)
})

test_that("each channel is filtered around its own strongest frequency", {
  L <- bandWave()
  R <- otherBandWave()
  filtered <- autoBandPass(tuneR::stereo(L, R))

  # The channels must choose different bands for the test to mean anything.
  expect_gt(peakFrequency(tuneR::channel(filtered, "left")), 1800)
  expect_lt(peakFrequency(tuneR::channel(filtered, "left")), 2200)
  expect_gt(peakFrequency(tuneR::channel(filtered, "right")), 3200)
  expect_lt(peakFrequency(tuneR::channel(filtered, "right")), 3600)

  # Each channel is what filtering that channel alone gives, up to the one
  # factor that normalises both channels to the peak of the recording.
  expect_equal(stats::cor(filtered@left, autoBandPass(L)@left), 1, tolerance=1e-6)
  expect_equal(stats::cor(filtered@right, autoBandPass(R)@left), 1, tolerance=1e-6)
  expect_equal(max(abs(c(filtered@left, filtered@right))), max(abs(c(L@left, R@left))))
})

test_that("a WaveMC keeps all of its channels", {
  # A WaveMC was refused outright, although the filter underneath can take one.
  L <- bandWave()
  R <- otherBandWave()
  w <- tuneR::WaveMC(cbind(L@left, R@left, L@left), samp.rate=L@samp.rate, bit=16)
  filtered <- autoBandPass(w)

  expect_s4_class(filtered, "WaveMC")
  expect_equal(ncol(filtered@.Data), 3)
  expect_equal(nrow(filtered@.Data), nrow(w@.Data))
  expect_equal(filtered@samp.rate, w@samp.rate)
  expect_equal(filtered@bit, w@bit)

  channel <- function(i) tuneR::Wave(filtered@.Data[,i], samp.rate=w@samp.rate, bit=16)
  expect_lt(peakFrequency(channel(1)), 2200)
  expect_gt(peakFrequency(channel(2)), 3200)
  expect_equal(filtered@.Data[,1], filtered@.Data[,3])
})

test_that("the settings reach every channel", {
  L <- bandWave()
  R <- otherBandWave()
  st <- tuneR::stereo(L, R)

  narrow <- autoBandPass(st, n.bw=1)
  wide <- autoBandPass(st, n.bw=3)
  # A wider band keeps more of the recording, in the second channel as well as
  # the first.
  expect_gt(sum(wide@left^2), sum(narrow@left^2))
  expect_gt(sum(wide@right^2), sum(narrow@right^2))

  # A low cut above the band of the left channel changes what that channel
  # chooses and leaves the right channel, whose band is above the cut, exactly
  # as it was: each channel is measured and filtered on its own.
  cut <- autoBandPass(st, lowcut=3000)
  expect_false(isTRUE(all.equal(cut@left, narrow@left)))
  expect_equal(cut@right, narrow@right)
})

test_that("the tagged classes keep their channels and their record", {
  L <- bandWave()
  R <- otherBandWave()

  tagged <- autoBandPass(tagWave(tuneR::stereo(L, R)))
  expect_s4_class(tagged, "TaggedWave")
  expect_true(tagged@stereo)
  expect_equal(length(tagged@processing), 1)
  expect_equal(tagged@processing[[1]]$func, "autoBandPass")

  w <- tuneR::WaveMC(cbind(L@left, R@left), samp.rate=L@samp.rate, bit=16)
  taggedMC <- autoBandPass(tagWave(w))
  expect_s4_class(taggedMC, "TaggedWaveMC")
  expect_equal(ncol(taggedMC@.Data), 2)
  expect_equal(length(taggedMC@processing), 1)
})

test_that("the band kept is normalised to the peak of the input", {
  # seewave's ffilter() returns what it passes at one part in its window length
  # of the level it had, so the band came back 60 dB down and, rounded to 16
  # bit PCM, as little more than zeros.
  wave <- bandWave()
  filtered <- autoBandPass(wave)
  expect_equal(max(abs(filtered@left)), max(abs(wave@left)))
})

test_that("a mono wave comes back in the format it was given", {
  # seewave returns 16 bit PCM whatever it is given, and a mono wave was
  # returned as seewave left it.
  wave <- bandWave()
  deep <- autoBandPass(tuneR::Wave(wave@left * 256, samp.rate=wave@samp.rate, bit=24))
  expect_equal(deep@bit, 24)
  expect_true(all(deep@left == round(deep@left)))

  float <- autoBandPass(tuneR::Wave(wave@left / 32768, samp.rate=wave@samp.rate, bit=32, pcm=FALSE))
  expect_equal(float@bit, 32)
  expect_false(float@pcm)
})
