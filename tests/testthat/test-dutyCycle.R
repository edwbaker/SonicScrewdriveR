test_that("Correct value is output", {
  # Half of full scale, so the signal is above the threshold the limit sets. The
  # samples were 0 and 1 in a 16 bit wave, which is silence either side of the
  # threshold once the limit is a proportion of full scale rather than of the
  # loudest sample the wave happens to hold.
  d <- data2Wave(c(rep_len(0,22050),rep_len(16384,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="unit", normalise=FALSE), 0.5)
  d <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_equal(dutyCycle(d, output="unit", normalise=TRUE, limit=0.5), 2/3)
})

test_that("Corect value is output in percantage mode", {
  d <- data2Wave(c(rep_len(0,22050),rep_len(16384,22050)), remove.offset=FALSE, normalise=FALSE)
  expect_equal(dutyCycle(d, output="percent", normalise=FALSE), 50)
})

# A pulse train with a duty cycle of exactly 0.1: a thousand of ten thousand
# samples at 8000, a quarter of 16 bit full scale, over a floor at 2000. The
# floor is below the threshold a limit of 0.1 sets (0.1 * 32767 = 3276.7) and
# the signal is above it.
pulseTrain <- function(click = FALSE) {
  x <- rep.int(c(rep.int(8000, 100), rep.int(2000, 900)), 10)
  # Inside the first burst, so the sample it replaces was already counted and a
  # reading that ignores it reports the same duty cycle as without it.
  if (click) x[50] <- 32767
  return(tuneR::Wave(x, samp.rate=8000, bit=16))
}

test_that("the duty cycle of a known pulse train is the proportion of it above the limit", {
  expect_equal(dutyCycle(pulseTrain(), limit=0.1), 0.1)
  expect_equal(dutyCycle(pulseTrain(), limit=0.1, output="percent"), 10)
})

test_that("one loud transient does not change the duty cycle around it", {
  # The threshold was a proportion of the loudest sample in the wave, so the
  # click set it for every other sample: the floor at 2000 cleared a threshold
  # of 0.1 * 8000 = 800 and the train measured 1 rather than 0.1, and adding the
  # click took it to 0.1 by raising the threshold above the floor. The figure
  # moved tenfold for a wave whose signal is unchanged.
  expect_equal(dutyCycle(pulseTrain(click=TRUE), limit=0.1), 0.1)
  expect_equal(
    dutyCycle(pulseTrain(click=TRUE), limit=0.1),
    dutyCycle(pulseTrain(click=FALSE), limit=0.1)
  )

  # What the old reading gave, so that the test fails if the threshold is taken
  # from the wave again.
  peakRelative <- function(w, limit) {
    s <- abs(w@left)
    return(sum(s > limit*max(s))/length(s))
  }
  expect_equal(peakRelative(pulseTrain(click=FALSE), 0.1), 1)
  expect_equal(peakRelative(pulseTrain(click=TRUE), 0.1), 0.1)
})

test_that("the limit is the same proportion whatever the wave's format", {
  # The same pulse train as a floating point wave, whose full scale is one
  # rather than 32767, so the proportions of full scale are identical.
  fs <- 32767
  x <- rep.int(c(rep.int(8000/fs, 100), rep.int(2000/fs, 900)), 10)
  float <- tuneR::Wave(x, samp.rate=8000, bit=32, pcm=FALSE)
  expect_equal(dutyCycle(float, limit=0.1), dutyCycle(pulseTrain(), limit=0.1))

  # A limit either side of the signal and the floor, in both formats.
  for (limit in c(0.05, 0.1, 0.3)) {
    expect_equal(dutyCycle(float, limit=limit), dutyCycle(pulseTrain(), limit=limit))
  }
  expect_equal(dutyCycle(pulseTrain(), limit=0.05), 1)
  expect_equal(dutyCycle(pulseTrain(), limit=0.3), 0)
})

test_that("a limit that is not one proportion of full scale is refused", {
  w <- pulseTrain()
  # An amplitude, which is what the help page described before the limit became
  # a proportion of full scale, would otherwise give a duty cycle of zero.
  expect_error(dutyCycle(w, limit=3000), "between 0 and 1")
  expect_error(dutyCycle(w, limit=-0.1), "between 0 and 1")
  expect_error(dutyCycle(w, limit=c(0.1, 0.2)), "single proportion")
  expect_error(dutyCycle(w, limit="0.1"), "single proportion")
})

test_that("Multi-channel input is analysed one channel at a time", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1500*(1:n)/sr) * 6000), samp.rate=sr, bit=16)

  # The single channel answers the multi-channel calls must reproduce.
  # allChannels() wraps each channel's result in a list of its own.
  left <- dutyCycle(L)
  right <- dutyCycle(R)
  expect_false(isTRUE(all.equal(left, right)))

  expect_equal(dutyCycle(tuneR::stereo(L, R)), list(list(left), list(right)))
  expect_equal(
    dutyCycle(tuneR::WaveMC(cbind(L@left, R@left, L@left), samp.rate=sr, bit=16)),
    list(list(left), list(right), list(left))
  )
})

test_that("Arguments are passed through to each channel", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1500*(1:n)/sr) * 6000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, R)

  # Both channels are above the limits used, so a channel's duty cycle says
  # which channel it was measured from rather than being zero either way.
  expect_equal(unlist(dutyCycle(st, output="percent")),
               c(dutyCycle(L, output="percent"), dutyCycle(R, output="percent")))
  expect_equal(unlist(dutyCycle(st, limit=0.15)),
               c(dutyCycle(L, limit=0.15), dutyCycle(R, limit=0.15)))
  expect_equal(unlist(dutyCycle(st, normalise=TRUE)),
               c(dutyCycle(L, normalise=TRUE), dutyCycle(R, normalise=TRUE)))
})

test_that("a wave holding NA samples is refused by name", {
  # The NA reached validateDutyCycle() as an NA duty cycle and came back as
  # "missing value where TRUE/FALSE needed", naming neither NA nor the wave.
  # The NA goes into the slot rather than through Wave(), so the test is of
  # dutyCycle() and not of tuneR's validity checking.
  w <- tuneR::Wave(c(rep.int(1, 500), rep.int(0, 500)), samp.rate=8000, bit=16)
  w@left[501] <- NA

  expect_error(dutyCycle(w, normalise=FALSE), "NA samples")
  expect_error(dutyCycle(w, normalise=TRUE), "NA samples")
  expect_error(dutyCycle(w, output="percent", normalise=FALSE), "NA samples")
})

# The same pulse train as pulseTrain() above, a duty cycle of exactly 0.1, in
# each format the package handles. Eight bit PCM is unsigned about a midpoint of
# 128, so its silence is 128 and its bursts are a quarter of full scale either
# side of that; every other format is signed about zero.
formatTrain <- function(bit, pcm=TRUE) {
  centre <- if (pcm && bit == 8) 128 else 0
  peak <- if (pcm && bit == 8) 127 else if (!pcm) 1 else 2^(bit - 1) - 1
  # Only an integer format wants rounding: round(0.25) is 0, which made the
  # floating point train silent and so measured a duty cycle of zero.
  on <- centre + if (pcm) round(0.25 * peak) else 0.25 * peak
  x <- rep.int(c(rep.int(on, 100), rep.int(centre, 900)), 10)
  return(tuneR::Wave(x, samp.rate=8000, bit=bit, pcm=pcm))
}

test_that("an eight bit wave's silence is its midpoint rather than zero", {
  # Eight bit PCM holds 0 to 255 about 128, so abs(wave@left) read its silence
  # as 128 and every sample cleared any threshold below half of full scale. A
  # train with a duty cycle of 0.1 measured 1 at every limit up to 0.5, and 0.1
  # at 0.6 only because the threshold happened to land between the two levels.
  w <- formatTrain(8)
  expect_equal(min(w@left), 128)
  expect_equal(dutyCycle(w, limit=0.1), 0.1)
  expect_equal(dutyCycle(w, limit=0.1, output="percent"), 10)

  # The bursts are a quarter of full scale, so a limit either side of that puts
  # everything or nothing above the threshold - which is what a limit should do,
  # rather than the 1 that every one of these gave.
  expect_equal(dutyCycle(w, limit=0.05), 0.1)
  expect_equal(dutyCycle(w, limit=0.3), 0)
  expect_equal(dutyCycle(w, limit=0.5), 0)
})

test_that("the same train measures the same duty cycle in every format", {
  # The point of a limit that is a proportion of full scale: one recording at
  # several depths is one duty cycle. Eight bit was the odd one out at 1.
  for (limit in c(0.05, 0.1, 0.3)) {
    measured <- c(
      "8" = dutyCycle(formatTrain(8), limit=limit),
      "16" = dutyCycle(formatTrain(16), limit=limit),
      "24" = dutyCycle(formatTrain(24), limit=limit),
      "32f" = dutyCycle(formatTrain(32, pcm=FALSE), limit=limit)
    )
    expect_equal(
      measured, c("8"=measured[["16"]], "16"=measured[["16"]],
                  "24"=measured[["16"]], "32f"=measured[["16"]]),
      info=paste("limit", limit)
    )
  }
})

test_that("an eight bit wave stays about its midpoint through normalise", {
  # normalize() from tuneR keeps an unsigned wave within 0 to 255 about its
  # midpoint, so the centring is right on this path too and not only on the raw
  # samples.
  n <- normalise(formatTrain(8))
  expect_gt(min(n@left), 0)
  expect_lt(min(n@left), 128)
  expect_lte(max(n@left), 255)

  # normalise=TRUE then measures 1 for this train, because normalize() removes
  # the offset and a wave that is nine tenths silent has one: the silence moves
  # off the centre and clears the threshold. That is what @param normalise warns
  # of and why it defaults to FALSE, and it is not a property of eight bit - the
  # signed depths do the same, which is what is asserted here.
  normalised <- vapply(
    list(formatTrain(8), formatTrain(16), formatTrain(32, pcm=FALSE)),
    function(w) dutyCycle(w, normalise=TRUE, limit=0.1),
    numeric(1)
  )
  expect_equal(normalised, c(1, 1, 1))
})

test_that("a limit above full scale counts nothing in any format", {
  # abs() of an eight bit wave's silence is 128, so limit=1 counted every
  # sample of one where it counts none of anything else.
  for (bit in c(8, 16, 24)) {
    expect_equal(dutyCycle(formatTrain(bit), limit=1), 0, info=paste("bit", bit))
  }
  expect_equal(dutyCycle(formatTrain(32, pcm=FALSE), limit=1), 0)
})
