# Shared fixtures for the pulse detectors, pulseIntervals() and pulseStats().
#
# Each wave holds a known number of square pulses at known sample positions, so
# the boundary convention written down at the top of R/pulseDetection.R can be
# checked against the truth rather than against another detector. The pulses are
# given as the onset and offset that convention calls for: the samples set are
# the onset up to but not including the offset, so offset - onset is the length
# of the pulse.
#
# The pulses are longer than the 480 sample window the energy detectors use by
# default, so all three detectors can see them, and the waves are long enough
# that the half window at each end leaves the middle of the wave clear.

pulseFixture <- function(pulses, n=8000) {
  x <- numeric(n)
  for (p in pulses) {
    x[p[1]:(p[2] - 1)] <- 1
  }
  list(
    onsets = vapply(pulses, function(p) p[1], numeric(1)),
    offsets = vapply(pulses, function(p) p[2], numeric(1)),
    wave = tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)
  )
}

# The cases that settle the convention between them. "one pulse" sits clear of
# both ends; the two edge cases are a pulse the wave starts in and a pulse the
# wave ends in; "two pulses one sample apart" is the shortest gap there can be;
# and a wave wholly above the threshold is both at once.
pulseFixtures <- function() {
  list(
    "no pulses" = pulseFixture(list()),
    "one pulse" = pulseFixture(list(c(1001, 1401))),
    "two pulses" = pulseFixture(list(c(1001, 1401), c(3001, 3401))),
    "a pulse starting at the first sample" = pulseFixture(list(c(1, 401))),
    "a pulse running to the last sample" = pulseFixture(list(c(7601, 8001))),
    "two pulses one sample apart" = pulseFixture(list(c(1001, 1401), c(1402, 1802))),
    "a wave wholly above the threshold" = pulseFixture(list(c(1, 8001)))
  )
}

# Stationary noise holds no pulses at all, however loud it is.
pulseFixtureNoise <- function(n=8000, sd=0.01, seed=1) {
  withr::with_seed(seed, {
    tuneR::Wave(stats::rnorm(n, sd=sd), samp.rate=44100, bit=32, pcm=FALSE)
  })
}

# A wave with one pulse in it and one sample that cannot be compared with any
# threshold.
pulseFixtureNA <- function(at=500) {
  f <- pulseFixture(list(c(1001, 1401)))
  f$wave@left[at] <- NA
  f
}

# Each detector, called so that all three are looking for the same pulses: the
# fixtures are square pulses at full scale, so half the peak magnitude puts
# pd_threshold() on the same pulses the energy detectors find.
#
# pd_dietrich2004() is called with filter=FALSE so that it is looking at the
# same signal as the other two. Its first step bandpass filters the wave at the
# loudest frequency within band, and the energy of a square pulse is at zero
# frequency rather than anywhere in the range of cricket song, so the filtered
# fixture is the ringing of a passband the pulse barely reaches and the three
# detectors would no longer be comparable. The filter has tests of its own in
# test-pulseDetection.R, on waves that hold the sort of sound it is for.
pulseDetectors <- function() {
  list(
    "pd_simple" = function(w) pd_simple(w),
    "pd_dietrich2004" = function(w) pd_dietrich2004(w, filter=FALSE),
    "pd_threshold" = function(w) pd_threshold(w, threshold=0.5)
  )
}

# The boundary convention at the top of R/pulseDetection.R, checked against any
# detector's result: onsets[i] and offsets[i] are the two ends of the same pulse,
# the only pulse that can be missing an offset is the last, and so every duration
# and every gap the result gives is positive.
expectPulseConvention <- function(r, info) {
  closed <- length(r$offsets)
  testthat::expect_true(
    (length(r$onsets) - closed) %in% c(0L, 1L),
    info = paste(info, "- one offset per onset, bar a pulse the wave ends in")
  )
  if (closed > 0) {
    testthat::expect_true(
      all(r$offsets > r$onsets[seq_len(closed)]),
      info = paste(info, "- every offset follows the onset it closes")
    )
  }
  if (length(r$onsets) > 1) {
    testthat::expect_true(
      all(r$onsets[-1] > r$offsets[seq_len(length(r$onsets) - 1)]),
      info = paste(info, "- one pulse ends before the next begins")
    )
  }
}
