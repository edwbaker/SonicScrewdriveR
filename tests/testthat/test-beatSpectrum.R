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
  #spectrum starts, and runs to the longest period asked for.
  expect_equal(min(bs$period), 2*0.005)
  expect_lte(max(bs$period), 1)
  expect_gt(max(bs$period), 1 / 2^(1/32))
  expect_true(all(diff(bs$period) > 0))
})

test_that("the spectrum stops at the longest period asked for", {
  skip_if_not_installed("WaveletComp")
  #The longest period was rounded up to a power of two of the shortest, so the
  #spectrum ran to 81.92 s for a max_period of 30, and the complexity could be
  #set by a period the caller had excluded.
  for (max_period in c(3, 30)) {
    bs <- beatSpectrum(rhythmWave(), max_period=max_period)
    expect_lte(max(bs$period), max_period)
    expect_gt(max(bs$period), max_period / 2^(1/32))
  }
  expect_true(all(beatComplexity(rhythmWave())$peak.periods <= 30))
})

test_that("a wave shorter than the shortest period is refused, naming the cause", {
  skip_if_not_installed("WaveletComp")
  #The envelope of such a wave is one or two points. runmed() only warned that
  #its window was longer than the wave, and the transform failed with "missing
  #value where TRUE/FALSE needed".
  tone <- function(n) tuneR::Wave(round(10000*sin(2*pi*50*(0:(n-1))/44100)), samp.rate=44100, bit=16)
  expect_error(
    beatSpectrum(tone(100), max_period=1),
    "The wave lasts 0.00227 s, shorter than twice min_period (0.01 s)", fixed=TRUE
  )
  expect_error(beatSpectrum(tone(440), max_period=1), "shorter than twice min_period")
  expect_s3_class(beatSpectrum(tone(441), max_period=1), "data.frame")
})

test_that("a longest period shorter than the spectrum can hold is refused", {
  expect_error(
    beatSpectrum(rhythmWave(), min_period=0.01, max_period=0.015),
    "max_period (0.015) must be at least twice min_period (0.01)", fixed=TRUE
  )
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

test_that("a flat-topped peak is found, at the middle of its top", {
  #Across a run of equal powers the sign of the difference goes from a rise to
  #nothing to a fall, never straight from one to the other, so the peak was
  #missed.
  local_mocked_bindings(
    beatSpectrum = function(wave, ...)
      data.frame(power=c(1, 2, 10, 10, 10, 2, 1, 5, 1), period=(1:9)*0.02)
  )
  bc <- beatComplexity(NULL)
  expect_equal(bc$peak.periods, c(0.08, 0.16))
  expect_identical(bc$num.peaks, 2L)
  expect_equal(bc$complexity, 0.16 / 2)

  #A top of an even number of powers is put at the first of the middle two.
  local_mocked_bindings(
    beatSpectrum = function(wave, ...)
      data.frame(power=c(1, 2, 10, 10, 2, 1), period=(1:6)*0.02)
  )
  expect_equal(beatComplexity(NULL)$peak.periods, 0.06)

  #A run of equal powers on the way up is not a peak.
  local_mocked_bindings(
    beatSpectrum = function(wave, ...)
      data.frame(power=c(1, 2, 2, 3, 1), period=(1:5)*0.02)
  )
  expect_equal(beatComplexity(NULL)$peak.periods, 0.08)
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

test_that("beatSpectrum analyses each channel of multi-channel input", {
  skip_if_not_installed("WaveletComp")
  # Only the left slot was read, so the stereo spectrum was the left channel's
  # and a WaveMC failed with a message naming a slot it does not have.
  L <- rhythmWave(cycle=0.1, seconds=4)
  R <- rhythmWave(cycle=0.2, seconds=4)

  left <- beatSpectrum(L, max_period=1)
  right <- beatSpectrum(R, max_period=1)
  # The channels must give different answers for the test to mean anything.
  expect_false(isTRUE(all.equal(left, right)))
  expect_false(isTRUE(all.equal(beatSpectrum(tuneR::stereo(L, R), max_period=1), left)))

  # beatSpectrum returns a data frame, so allChannels() does not wrap it again.
  expect_equal(beatSpectrum(tuneR::stereo(L, R), max_period=1), list(left, right))
  expect_equal(
    beatSpectrum(
      tuneR::WaveMC(cbind(L@left, R@left, L@left), samp.rate=L@samp.rate, bit=16),
      max_period=1
    ),
    list(left, right, left)
  )
})

test_that("beatSpectrum finds the rhythm of each channel separately", {
  skip_if_not_installed("WaveletComp")
  L <- rhythmWave(cycle=0.1, seconds=4)
  R <- rhythmWave(cycle=0.2, seconds=4)

  bs <- beatSpectrum(tuneR::stereo(L, R), max_period=1)
  expect_equal(bs[[1]]$period[which.max(bs[[1]]$power)], 0.1, tolerance=0.05)
  expect_equal(bs[[2]]$period[which.max(bs[[2]]$power)], 0.2, tolerance=0.05)
})

test_that("beatComplexity measures each channel of multi-channel input", {
  skip_if_not_installed("WaveletComp")
  L <- rhythmWave(cycle=0.1, seconds=4)
  R <- rhythmWave(cycle=0.2, seconds=4)

  left <- beatComplexity(L)
  right <- beatComplexity(R)
  expect_false(isTRUE(all.equal(left, right)))
  expect_false(isTRUE(all.equal(beatComplexity(tuneR::stereo(L, R)), left)))

  # beatComplexity returns a list, so allChannels() does not wrap it again.
  expect_equal(beatComplexity(tuneR::stereo(L, R)), list(left, right))
  expect_equal(
    beatComplexity(tuneR::WaveMC(cbind(L@left, R@left), samp.rate=L@samp.rate, bit=16)),
    list(left, right)
  )
})

test_that("beatSpectrum passes its arguments to each channel", {
  skip_if_not_installed("WaveletComp")
  L <- rhythmWave(cycle=0.1, seconds=4)
  R <- rhythmWave(cycle=0.2, seconds=4)
  st <- tuneR::stereo(L, R)

  fine <- beatSpectrum(st, max_period=1, dj=1/32)
  coarse <- beatSpectrum(st, max_period=1, dj=1/8)
  expect_gt(nrow(fine[[1]]), nrow(coarse[[1]]))
  expect_gt(nrow(fine[[2]]), nrow(coarse[[2]]))
  expect_equal(min(beatSpectrum(st, min_period=0.02, max_period=1)[[2]]$period), 2*0.02)
})
