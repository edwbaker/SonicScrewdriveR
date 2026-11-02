# The pulse boundary convention written down at the top of R/pulseDetection.R,
# checked against the shared fixtures in helper-pulseFixtures.R.

test_that("every detector reports onsets and offsets that pair", {
  # pd_dietrich2004() suppressed the onset of a pulse already in progress at the
  # first analysed sample while still reporting its offset, and pd_threshold()
  # thinned the onsets and the offsets in two independent passes, so both
  # returned lists that described different pulses and pulseStats() gave negative
  # durations from them.
  fixtures <- pulseFixtures()
  for (detector in names(pulseDetectors())) {
    for (case in names(fixtures)) {
      r <- pulseDetectors()[[detector]](fixtures[[case]]$wave)
      expectPulseConvention(r, paste(detector, "-", case))
    }
  }
})

test_that("pd_threshold reports exactly the pulses the fixtures hold", {
  # pd_threshold() thresholds the samples themselves rather than a windowed
  # energy, so its boundaries are the truth and not an approximation to it.
  fixtures <- pulseFixtures()
  for (case in names(fixtures)) {
    f <- fixtures[[case]]
    r <- pd_threshold(f$wave, threshold=0.5)
    expect_equal(r$onsets, as.integer(f$onsets), info=case)
    #A pulse the wave ends in has no offset, as there is no sample after it.
    closed <- f$offsets[f$offsets <= length(f$wave)]
    expect_equal(r$offsets, as.integer(closed), info=case)
  }
})

test_that("the energy detectors report each fixture at the same boundaries", {
  # The boundaries of a windowed energy are displaced outward from the truth, so
  # these are pinned rather than compared with the fixture: what matters is that
  # they are where the detector's own pulse vector says the pulse is, and that
  # the edge cases are reported at all.
  fixtures <- pulseFixtures()
  expected <- list(
    "no pulses" = list(onsets=integer(0), offsets=integer(0)),
    "one pulse" = list(onsets=955L, offsets=1447L),
    "two pulses" = list(onsets=c(955L, 2955L), offsets=c(1447L, 3447L)),
    #A pulse the wave starts in is reported from the first analysed sample, which
    #is as early as a window of 480 samples can be centred.
    "a pulse starting at the first sample" = list(onsets=241L, offsets=447L),
    #A pulse still in progress at the last analysed sample has no offset.
    "a pulse running to the last sample" = list(onsets=7555L, offsets=integer(0)),
    #A gap of one sample is far shorter than the window, so the two pulses are
    #reported as one. This is the cost of the window, and is documented.
    "two pulses one sample apart" = list(onsets=955L, offsets=1848L),
    #Nothing stands out from the floor of a wave of constant amplitude, so an
    #adaptive threshold finds no pulse in one. pd_threshold(), which takes a
    #fraction of the peak instead, reports the whole wave as one pulse.
    "a wave wholly above the threshold" = list(onsets=integer(0), offsets=integer(0))
  )
  for (case in names(fixtures)) {
    r <- pd_simple(fixtures[[case]]$wave)
    expect_equal(r[c("onsets", "offsets")], expected[[case]], info=case)
  }
})

test_that("an onset from pd_simple is a sample its own pulse vector marks as a pulse", {
  # The onset was the last sample below the threshold and the offset the last
  # sample above it, so the two ends of one pulse were read off two different
  # conventions and the onset was not a sample pulse marks at all. The durations
  # came out right because the two errors cancelled.
  fixtures <- pulseFixtures()
  for (case in names(fixtures)) {
    r <- pd_simple(fixtures[[case]]$wave)
    if (length(r$onsets) > 0) {
      expect_true(all(r$pulse[r$onsets]), info=case)
    }
    if (length(r$offsets) > 0) {
      #An offset is the first sample after the pulse, so it is not a pulse sample
      #and the sample before it is.
      expect_false(any(r$pulse[r$offsets]), info=case)
      expect_true(all(r$pulse[r$offsets - 1]), info=case)
    }
  }
})

test_that("the three detectors no longer disagree by one sample about one pulse", {
  # pd_simple() reported 954 where pd_threshold() and pd_dietrich2004() reported
  # the first sample above the threshold, so onsets were not comparable between
  # methods even on identical input.
  f <- pulseFixtures()[["one pulse"]]
  expect_equal(pd_threshold(f$wave, threshold=0.5)$onsets, 1001L)
  expect_equal(pd_simple(f$wave)$onsets, 955L)
  expect_equal(pd_dietrich2004(f$wave, filter=FALSE)$onsets, 953L)
  #What the two still differ by is the threshold each reports a boundary at,
  #rather than the one sample the two conventions used to differ by. pd_simple()
  #thresholds against F_a, while pd_dietrich2004() reports the F_1 crossing that
  #opened the pulse and uses F_a only to confirm it, so its boundaries sit two
  #samples further out at each end: F_1 is below F_a and so is crossed earlier
  #on the way up and later on the way down.
  expect_equal(pd_simple(f$wave)$offsets, 1447L)
  expect_equal(pd_dietrich2004(f$wave, filter=FALSE)$offsets, 1449L)
})

test_that("a sample exactly equal to the threshold does not lose a crossing", {
  # above and below were both strict, so a sample exactly equal to the threshold
  # was on neither side of it and a crossing through one was detected in neither
  # direction: this wave returned no onsets and one offset. The threshold is a
  # fraction of the peak, so a plateau at a round fraction of the peak hits it
  # exactly.
  w <- tuneR::Wave(c(rep(0, 10), rep(0.5, 10), rep(1, 10), rep(0, 10)),
                   samp.rate=44100, bit=32, pcm=FALSE)

  r <- pd_threshold(w, threshold=0.5)
  expect_equal(r$onsets, 21L)
  expect_equal(r$offsets, 31L)
  #Squaring the magnitudes moves the plateau off the threshold, so the two modes
  #used to disagree about how many pulses the wave holds. They now agree.
  expect_equal(pd_threshold(w, threshold=0.5, pd=TRUE)[c("onsets", "offsets")],
               r[c("onsets", "offsets")])
})

test_that("a threshold of zero is a threshold like any other", {
  # Every magnitude is at or above zero, and below was a strict mag < 0, so no
  # sample was below the threshold and no onset could be found at all.
  x <- numeric(100)
  x[11:20] <- 1
  w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)

  r <- pd_threshold(w, threshold=0)
  expect_equal(r$onsets, 11L)
  expect_equal(r$offsets, 21L)
})

test_that("pd_threshold keeps or discards a closely spaced pulse whole", {
  # The onsets and the offsets were thinned in two independent passes. The
  # spacing of the onsets is the pulse period, while the spacing of the offsets
  # is the period plus the change in duration, so the two passes kept different
  # pulses: this wave returned the third pulse's onset beside the second pulse's
  # offset, and a duration of -10 samples.
  x <- numeric(1000)
  x[101:110] <- 1
  x[121:160] <- 1
  x[171:180] <- 1
  w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)

  truth <- pd_threshold(w, threshold=0.5, U=1)
  expect_equal(truth$onsets, c(101L, 121L, 171L))
  expect_equal(truth$offsets, c(111L, 161L, 181L))

  debounced <- pd_threshold(w, threshold=0.5, U=30)
  #The second pulse begins 20 samples after the first, so it is discarded with
  #its own offset rather than leaving that offset behind.
  expect_equal(debounced$onsets, c(101L, 171L))
  expect_equal(debounced$offsets, c(111L, 181L))
  expect_true(all(pulseStats(debounced)$durations > 0))
})

test_that("pd_dietrich2004 reports a pulse already in progress at the first analysed sample", {
  # The onset of such a pulse was suppressed while the offset scan still reported
  # its end, so the result held one more offset than onset and every later pulse
  # was paired with the previous pulse's offset: these three 400 sample pulses
  # gave durations of -2505 and -1505 samples.
  x <- numeric(8000)
  x[1:400] <- 1
  x[3001:3400] <- 1
  x[5001:5400] <- 1
  w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)

  r <- pd_dietrich2004(w, filter=FALSE)
  #241 is the first analysed sample, which is where a pulse already in progress
  #there is reported.
  expect_equal(r$onsets, c(241L, 2953L, 4953L))
  expect_equal(r$offsets, c(449L, 3449L, 5449L))
  expect_true(all(pulseStats(r)$durations > 0))
})

test_that("the two readings of Dietrich (2004) agree about a wave of constant amplitude", {
  # pd_dietrich2004() reported neither an onset nor an offset for such a wave,
  # because the single run of energy above the threshold began at the first
  # analysed sample and was suppressed, while pd_simple() reported one pulse
  # spanning the whole of it. Both now report none: theta is the floor of the
  # wave, nothing in it rises above the floor, and so there is no pulse to find.
  w <- tuneR::Wave(rep(1, 8000), samp.rate=44100, bit=32, pcm=FALSE)

  simple <- pd_simple(w)
  dietrich <- pd_dietrich2004(w, filter=FALSE)
  expect_length(simple$onsets, 0)
  expect_length(simple$offsets, 0)
  expect_length(dietrich$onsets, 0)
  expect_length(dietrich$offsets, 0)
  expect_false(any(simple$pulse))
})

test_that("a silent wave is one pulse to pd_dietrich2004 and none to the others", {
  # Where the two readings part company. The comparisons of Algorithm 1 are not
  # strict, and every energy of a silent wave is exactly equal to both
  # thresholds, so the paper's own test puts the whole analysed range inside a
  # pulse. The convention at the top of R/pulseDetection.R has a sample equal to
  # the threshold outside the pulse, which is what the other two detectors do,
  # so this is the one case in which the three disagree by more than a sample.
  silent <- tuneR::Wave(numeric(8000), samp.rate=44100, bit=32, pcm=FALSE)

  dietrich <- pd_dietrich2004(silent, filter=FALSE)
  #241 is the first analysed sample. The pulse is still open at the last one, so
  #it has an onset and no offset, which the convention allows.
  expect_equal(dietrich$onsets, 241L)
  expect_length(dietrich$offsets, 0)
  expect_length(pd_simple(silent)$onsets, 0)
  expect_length(pd_threshold(silent, threshold=0.5)$onsets, 0)
})

test_that("theta is taken from the energy ste() measured, not from the padding it leaves", {
  # theta is meant to sit a fraction gamma of the way up from the noise floor,
  # but ste() leaves the half window at each end of its output as a zero, so
  # min(e_u) was zero whatever the recording held and theta collapsed to
  # gamma*max(e_u). On this wave of stationary noise that put theta 13 times
  # below the real floor and reported one pulse spanning samples 60 to 7940,
  # 7880 of the 8000 samples, with both boundaries at the edge of the padding.
  w <- pulseFixtureNoise()
  e <- ste(w, U=120)
  analysed <- 241:7760

  expect_equal(min(e), 0)
  expect_gt(min(e[analysed]), 0.75)

  r <- pd_simple(w)
  expect_equal(r$theta, min(e[analysed]) + 0.05*(max(e[analysed]) - min(e[analysed])))
  expect_gt(r$theta, min(e[analysed]))
  #No pulse spans the recording, and no boundary sits at the edge of the padding,
  #which is exactly floor(U/2) and n - floor(U/2).
  expect_false(any(r$onsets == 60))
  expect_false(any(r$offsets == 7940))
  expect_true(all(r$offsets - r$onsets[seq_along(r$offsets)] < 1000))
  #What is left is the envelope of the noise fluctuating about its own floor,
  #which is short pulses rather than one long one. A minimum duration would
  #remove them, and the package has none.
  expect_gt(length(r$onsets), 1)
})

test_that("no pulse is reported in the half window at each end of a wave", {
  # The boundaries pd_simple() gave for a pulse at the very start or end of a
  # wave were the edges of the padding ste() leaves rather than anything in the
  # signal: a pulse occupying samples 1 to 400 was reported as starting at 60,
  # and one occupying 7601 to 8000 as ending at 7940.
  starts <- pulseFixtures()[["a pulse starting at the first sample"]]
  ends <- pulseFixtures()[["a pulse running to the last sample"]]

  expect_equal(pd_simple(starts$wave)$onsets, 241L)
  expect_equal(pd_simple(ends$wave)$offsets, integer(0))
  expect_equal(pd_simple(ends$wave)$onsets, 7555L)
})

test_that("the energy detectors overstate a duration by part of a window", {
  # Documented under pd_simple(). The energy rises over the half window before a
  # pulse begins and falls over the half window after it ends, so each boundary
  # is displaced outward and the duration is overstated. Pinned here because the
  # bias depends on gamma, alpha and the amplitude as well as on U, so it is not
  # the whole window it might be taken for: at U=120 it is 92 samples for
  # pd_simple() and 96 for pd_dietrich2004(), against a pulse of exactly 400.
  # pd_dietrich2004() overstates by the more of the two because it reports the
  # F_1 crossing at each end, and F_1 is the lower of its two thresholds.
  f <- pulseFixtures()[["one pulse"]]
  truth <- 400

  exact <- pd_threshold(f$wave, threshold=0.5)
  expect_equal(exact$offsets - exact$onsets, truth)
  simple <- pd_simple(f$wave)
  expect_equal(simple$offsets - simple$onsets, truth + 92)
  dietrich <- pd_dietrich2004(f$wave, filter=FALSE)
  expect_equal(dietrich$offsets - dietrich$onsets, truth + 96)
  #The bias shrinks with the window, which is the time resolution asked for.
  narrow <- pd_simple(f$wave, U=20, V=80)
  expect_lt(narrow$offsets - narrow$onsets - truth, 92)
})

test_that("a wave holding a sample that cannot be compared gives no pulses", {
  # ste() carries an NA sample forward through its cumulative sum, so there is no
  # energy left to threshold and no noise floor to measure. pd_threshold()
  # refuses such a wave outright; the energy detectors report nothing, rather
  # than a pulse one end of which is the NA.
  f <- pulseFixtureNA()

  expect_error(pd_threshold(f$wave, threshold=0.5), "NA samples")
  for (detector in c("pd_simple", "pd_dietrich2004")) {
    r <- do.call(detector, list(f$wave))
    expect_identical(r$theta, NA_real_, info=detector)
    expect_length(r$onsets, 0)
    expect_length(r$offsets, 0)
  }
})

test_that(".pulseRuns reports a run as its first sample and the sample after it", {
  expect_equal(.pulseRuns(logical(0)), list(onsets=integer(0), offsets=integer(0)))
  expect_equal(.pulseRuns(c(TRUE, TRUE, FALSE, TRUE)),
               list(onsets=c(1L, 4L), offsets=3L))
  #A sample that cannot be compared with the threshold is not part of a pulse,
  #rather than opening one at one end of the comparison and closing it at the
  #other.
  expect_equal(.pulseRuns(c(FALSE, NA, TRUE, TRUE, FALSE)),
               list(onsets=3L, offsets=5L))
  expect_equal(.pulseRuns(c(FALSE, FALSE)), list(onsets=integer(0), offsets=integer(0)))
})

test_that(".pulsePairs pairs each onset with the offset that closes it", {
  expect_equal(.pulsePairs(c(100L, 300L), c(200L, 400L)),
               list(onsets=c(100L, 300L), offsets=c(200L, 400L)))
  #Two onsets before the same offset describe one pulse, because the scan found
  #no boundary between them.
  expect_equal(.pulsePairs(c(100L, 200L), 250L),
               list(onsets=100L, offsets=250L))
  #An offset before the first onset closes a pulse that was never opened.
  expect_equal(.pulsePairs(100L, c(50L, 200L)),
               list(onsets=100L, offsets=200L))
  expect_equal(.pulsePairs(integer(0), 50L),
               list(onsets=integer(0), offsets=integer(0)))
  #Nothing closes the last pulse, which was still in progress at the last
  #analysed sample.
  expect_equal(.pulsePairs(c(100L, 300L), 200L),
               list(onsets=c(100L, 300L), offsets=200L))
  expect_equal(.pulsePairs(100L, integer(0)),
               list(onsets=100L, offsets=integer(0)))
})

test_that(".steDefined gives the samples that hold a whole window", {
  expect_equal(.steDefined(10, 20), integer(0))
  expect_equal(.steDefined(0, 2), integer(0))
  expect_equal(.steDefined(100, 4), 3:98)
  #For an odd window the extra sample is taken from after the centre.
  expect_equal(.steDefined(100, 5), 3:97)
  expect_equal(range(.steDefined(8000, 480)), c(241, 7760))
})
