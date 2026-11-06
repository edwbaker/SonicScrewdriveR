test_that("Reject incorrect unit", {
  expect_error(region("monkeys", 0, 10), "Unit must be one of samples, seconds, minutes, hours")
})

test_that("TimeRegion with samples", {
  tr <- region("samples", 0, 10)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 10)
  expect_equal(tr@unit, "samples")
  # Sample numbers start at one, as they do for every other unit.
  expect_equal(.timeRegion2samples(tr, 44100), c(1,10))
})

test_that("TimeRegion with seconds", {
  tr <- region("seconds", 0, 10)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 10)
  expect_equal(tr@unit, "seconds")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*10))
})

test_that("TimeRegion with minutes", {
  tr <- region("minutes", 0, 1)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 1)
  expect_equal(tr@unit, "minutes")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*60))
})

test_that("TimeRegion with hours", {
  tr <- region("hours", 0, 1)
  expect_true(inherits(tr, "TimeRegion"))
  expect_equal(tr@from, 0)
  expect_equal(tr@to, 1)
  expect_equal(tr@unit, "hours")
  expect_equal(.timeRegion2samples(tr, 44100), c(1,44100*60*60))
})

test_that("Subsetting Wave by TimeRegion works", {
  w <- tuneR::sawtooth(440, duration=2*44100, samp.rate=44100, stereo=TRUE)
  w <- w[region("seconds", 1, 2)]
  expect_equal(length(w@left), 44100)
  expect_equal(length(w@right), 44100)
})

test_that("a region in time holds the samples that begin within it", {
  # Both ends were taken as sample numbers and both kept, so a second was one
  # sample too long and consecutive regions shared a sample.
  w <- tuneR::Wave(as.numeric(1:1000), samp.rate=100, bit=32, pcm=FALSE)
  expect_equal(w[region("seconds", 0, 1)]@left, 1:100)
  expect_equal(w[region("seconds", 1, 2)]@left, 101:200)
  expect_equal(w[region("seconds", 2, 3)]@left, 201:300)
  expect_equal(.timeRegion2samples(region("minutes", 1, 2), 1), c(61, 120))
  expect_equal(.timeRegion2samples(region("hours", 1, 2), 1), c(3601, 7200))

  # A bound between two samples is placed rather than cut short: the span of
  # one sample from 0.015 s holds the sample beginning at 0.02 s.
  expect_equal(w[region("seconds", 0.015, 0.025)]@left, 3)
  # 0.07 * 100 is 7.000000000000001, which is not past the eighth sample.
  expect_equal(.timeRegion2samples(region("seconds", 0.07, 0.1), 100), c(8, 10))

  # A region of no length holds no samples.
  expect_length(w[region("seconds", 1, 1)]@left, 0)
  # A region beginning after the wave ends is still refused.
  expect_error(w[region("seconds", 10, 11)], "Time region begins after the end of the wave.")
})

test_that("a region in samples holds the samples it names", {
  w <- tuneR::Wave(as.numeric(1:1000), samp.rate=100, bit=32, pcm=FALSE)
  expect_equal(w[region("samples", 101, 200)]@left, 101:200)
  expect_equal(w[region("samples", 5, 5)]@left, 5)
})

test_that("0 and Inf are handled correctly", {
  w <- tuneR::sawtooth(440, duration=2*44100, samp.rate=44100, stereo=TRUE)
  w <- w[region("seconds", 0, Inf)]
  expect_equal(length(w@left), 88200)
})

test_that("a TimeRegion that cannot be resolved is refused when it is built", {
  #The class had no validity method, so new() accepted anything the slot types
  #allowed and region() checked only its time.unit. A region running backwards
  #survived until `[` reported "Time region begins after the end of the wave.",
  #which is false and points at the wave rather than at the region.
  expect_error(
    new("TimeRegion", from=10, to=1, unit="seconds"),
    "cannot end before it begins"
  )
  expect_error(region("seconds", 10, 1), "cannot end before it begins")
  expect_error(
    new("TimeRegion", from=1, to=2, unit="furlongs"),
    "unit must be one of samples, seconds, minutes, hours"
  )
  expect_error(
    new("TimeRegion", from=-5, to=10, unit="seconds"),
    "from cannot be negative"
  )
  expect_error(
    new("TimeRegion", from=NA_real_, to=10, unit="seconds"),
    "from must be a single number"
  )
  expect_error(
    new("TimeRegion", from=c(1, 2), to=10, unit="seconds"),
    "from must be a single number"
  )
})

test_that("the regions the package already builds are still valid", {
  expect_silent(validObject(region("samples", 0, 10)))
  expect_silent(validObject(region("seconds", 0, Inf)))
  expect_silent(validObject(region("seconds", 1, 2)))
  expect_silent(validObject(region("minutes", 0, 1)))
  expect_silent(validObject(region("hours", 0, 1)))
  #region()'s own defaults, and the class prototype
  expect_silent(validObject(region("seconds")))
  expect_silent(validObject(new("TimeRegion")))
  #A region of no length is a region, so it is not refused
  expect_silent(validObject(new("TimeRegion", from=5, to=5, unit="samples")))
})

test_that("cutting a tagged wave to a region is recorded", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  tw <- tagWave(w, origin="test")
  cut <- tw[region("seconds", 0.25, 0.5)]
  expect_s4_class(cut, "TaggedWave")
  expect_equal(cut@origin, "test")
  expect_equal(cut@left, w[region("seconds", 0.25, 0.5)]@left)
  expect_equal(cut@right, w[region("seconds", 0.25, 0.5)]@right)
  expect_length(cut@processing, 1)
  rec <- cut@processing[[1]]
  expect_identical(rec$func, "[")
  expect_equal(rec$params, list(from=0.25, to=0.5, time.unit="seconds"))
  expect_equal(rec$input_hash, .waveHash(tw, rec$hash_method))
  expect_equal(rec$output_hash, .waveHash(cut, rec$hash_method))
  # A cut of a cut adds a record of its own.
  again <- cut[region("samples", 1, 10)]
  expect_length(again@processing, 2)
  expect_equal(again@processing[[2]]$params, list(from=1, to=10, time.unit="samples"))
  # A plain wave is cut without a record, and a region past its end is refused
  # as before.
  expect_false(.isTagged(w[region("seconds", 0.25, 0.5)]))
  expect_error(tw[region("seconds", 2, 3)], "Time region begins after the end of the wave.")
})
