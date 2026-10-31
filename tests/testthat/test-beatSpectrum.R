#A train of short pulses with a cycle of a tenth of a second, so the rhythm of
#the wave is known and can be looked for in the spectrum.
rhythmWave <- function(cycle=0.1, samp.rate=2000, seconds=10) {
  n.cycle <- round(cycle*samp.rate)
  pulse <- c(rep(1, round(n.cycle/10)), rep(0, n.cycle - round(n.cycle/10)))
  tuneR::Wave(
    round(10000*rep_len(pulse, samp.rate*seconds)),
    samp.rate=samp.rate, bit=16
  )
}

test_that("a power for each period is returned", {
  skip_if_not_installed("WaveletComp")
  bs <- beatSpectrum(rhythmWave(), max_period=1)
  expect_s3_class(bs, "data.frame")
  expect_equal(colnames(bs), c("power", "period"))
  expect_gt(nrow(bs), 0)
  expect_true(all(is.finite(bs$power)))
})

test_that("the periods are reported in seconds and run from the shortest asked for", {
  skip_if_not_installed("WaveletComp")
  bs <- beatSpectrum(rhythmWave(), min_period=0.005, max_period=1)
  #The transform starts at two of the shortest periods, so that is where the
  #spectrum starts, and runs past the longest period asked for.
  expect_equal(min(bs$period), 2*0.005)
  expect_gte(max(bs$period), 1)
  expect_true(all(diff(bs$period) > 0))
})

test_that("the rhythm of the wave is the strongest period in the spectrum", {
  skip_if_not_installed("WaveletComp")
  bs <- beatSpectrum(rhythmWave(cycle=0.1), max_period=1)
  expect_equal(bs$period[which.max(bs$power)], 0.1, tolerance=0.05)
})

test_that("a different rhythm gives a different strongest period", {
  skip_if_not_installed("WaveletComp")
  bs <- beatSpectrum(rhythmWave(cycle=0.2), max_period=1)
  expect_equal(bs$period[which.max(bs$power)], 0.2, tolerance=0.05)
})

test_that("the shortest period asked for sets the resolution of the envelope", {
  skip_if_not_installed("WaveletComp")
  coarse <- beatSpectrum(rhythmWave(), min_period=0.02, max_period=1)
  expect_equal(min(coarse$period), 2*0.02)
})

test_that("more voices per octave give a finer spectrum", {
  skip_if_not_installed("WaveletComp")
  fine <- beatSpectrum(rhythmWave(), max_period=1, dj=1/32)
  coarse <- beatSpectrum(rhythmWave(), max_period=1, dj=1/8)
  expect_gt(nrow(fine), nrow(coarse))
})

test_that("the complexity, its peaks and their number are returned", {
  skip_if_not_installed("WaveletComp")
  bc <- beatComplexity(rhythmWave())
  expect_named(bc, c("complexity", "peak.periods", "num.peaks"))
  expect_equal(bc$num.peaks, length(bc$peak.periods))
  expect_gt(bc$num.peaks, 0)
})

test_that("the complexity is the longest repeating period over the peak count", {
  skip_if_not_installed("WaveletComp")
  bc <- beatComplexity(rhythmWave())
  expect_equal(bc$complexity, max(bc$peak.periods) / bc$num.peaks)
})

test_that("the peaks found are peaks of the beat spectrum", {
  skip_if_not_installed("WaveletComp")
  wave <- rhythmWave()
  bc <- beatComplexity(wave)
  bs <- beatSpectrum(wave)
  expect_true(all(bc$peak.periods %in% bs$period))
  #Each is higher than the period either side of it.
  at <- match(bc$peak.periods, bs$period)
  inner <- at[at > 1 & at < nrow(bs)]
  expect_true(all(bs$power[inner] > bs$power[inner-1]))
  expect_true(all(bs$power[inner] > bs$power[inner+1]))
})

test_that("the rhythm of the wave is among the peaks found", {
  skip_if_not_installed("WaveletComp")
  bc <- beatComplexity(rhythmWave(cycle=0.1))
  expect_true(any(abs(bc$peak.periods - 0.1) < 0.005))
})

test_that("plotting does not change the measurements", {
  skip_if_not_installed("WaveletComp")
  wave <- rhythmWave()
  expect_equal(beatComplexity(wave, plot=TRUE), beatComplexity(wave, plot=FALSE))
})

test_that("a spectrum with no peak gives an undefined complexity, not -Inf", {
  #A monotonic spectrum has no strict local maximum, so there is no longest
  #repeating period to divide by the peak count. max() of nothing returned -Inf
  #with a bare warning, and -Inf/0 was reported as the complexity.
  local_mocked_bindings(
    beatSpectrum = function(wave, ...) data.frame(power=1:10, period=(1:10)*0.005)
  )
  expect_silent(bc <- beatComplexity(NULL))
  expect_identical(bc$num.peaks, 0L)
  expect_identical(bc$peak.periods, numeric(0))
  expect_identical(bc$complexity, NA_real_)
})

test_that("a spectrum too short to hold a peak gives an undefined complexity", {
  for (power in list(5, c(1, 2))) {
    local_mocked_bindings(
      beatSpectrum = function(wave, ...)
        data.frame(power=power, period=seq_along(power)*0.005)
    )
    expect_silent(bc <- beatComplexity(NULL))
    expect_identical(bc$complexity, NA_real_)
    expect_identical(bc$num.peaks, 0L)
  }
})
