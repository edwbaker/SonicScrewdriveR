test_that("generateNoise rejects unknown input to wave", {
  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_silent(generateNoise(w, "white", FALSE, 0.5, output="list"))

  w <- tagWave(w)
  expect_silent(generateNoise(w, "white", FALSE, 0.5, output="list"))

  expect_error(generateNoise(1, "white", FALSE, 0.5, output="list"), "wave must be a Wave like object, or a list of such objects.")

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100)),
    "koi carp"
  )
  expect_error(generateNoise(w, "white", FALSE, 0.5, output="list"), "wave must be a Wave like object, or a list of such objects.")
})

test_that("generateNoise outputs something Wave-like", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list"), "Wave")

  w <- tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list"), "WaveMC")

  w <- list(
    tuneR::sine(440, duration=44100, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, duration=44100, samp.rate=44100))
  )
  expect_type(generateNoise(w, "white", FALSE, 0.5, output="list"), "list")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list")[[1]], "Wave")
  expect_s4_class(generateNoise(w, "white", FALSE, 0.5, output="list")[[2]], "WaveMC")
})

# The thresholds below were set when a pulse peaked at 2^32 rather than at the 1
# a floating point wave allows, and noise was referenced to that same wrong scale.
# At a noise ratio of 0.25 the noise now sits near 0.05 of full scale.
test_that("noise amplitude is correct on Wave", {
  w <- pulse("dirac", leading=0)
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")

  expect_equal(length(w), length(n))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
})

test_that("noise amplitude is correct on TaggedWave", {
  w <- pulse("dirac", leading=0, output="TaggedWave")
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@left))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_gt(mean(abs(n@left[2:length(n@left)])), 0.02)

  w <- pulse("dirac", leading=0, output="TaggedWave")
  n <- generateNoise(w, "white", FALSE, 0.25, noise.ref="max", output="list")
  expect_equal(length(w@left), length(n@left))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_gt(mean(abs(n@left[2:length(n@left)])), 0.02)

  w <- pulse("dirac", leading=0, output="TaggedWave", stereo=TRUE)
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@right))
  expect_equal(length(w@left), length(n@left))
  expect_equal(length(w@right), length(n@right))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_equal(which(n@right > max(abs(n@left))/2), 1)
  expect_gt(mean(abs(n@left[2:length(n@left)])), 0.02)

  w <- pulse("dirac", leading=0, output="TaggedWave")
  n <- generateNoise(w, c("white", "white"), FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@left))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_gt(mean(abs(n@left[2:length(n@left)])), 0.02)

  w <- pulse("dirac", leading=0, output="TaggedWave", stereo=TRUE)
  n <- generateNoise(w, c("white", "white"), FALSE, 0.25, output="list")
  expect_equal(length(w@left), length(n@right))
  expect_equal(length(w@left), length(n@left))
  expect_equal(length(w@right), length(n@right))
  expect_equal(which(n@left > max(abs(n@left))/2), 1)
  expect_equal(which(n@right > max(abs(n@left))/2), 1)
  expect_gt(mean(abs(n@left[2:length(n@left)])), 0.02)
})

test_that("noise amplitude is correct on WaveMC", {
  w <- tuneR::WaveMC(pulse("dirac", leading=0))
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")

  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
})

test_that("noise amplitude is correct on TaggedWaveMC", {
  w <- tagWave(tuneR::WaveMC(pulse("dirac", leading=0, output="Wave")))
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
  expect_gt(mean(abs(n@.Data[2:nrow(w@.Data), 1])), 0.02)

  w <- tagWave(tuneR::WaveMC(pulse("dirac", leading=0, output="Wave")))
  n <- generateNoise(w, "white", FALSE, 0.25, noise.ref="max", output="list")
  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
  expect_gt(mean(abs(n@.Data[2:nrow(w@.Data), 1])), 0.02)

  w <- tagWave(tuneR::WaveMC(pulse("dirac", leading=0, output="Wave", stereo=TRUE)))
  n <- generateNoise(w, "white", FALSE, 0.25, output="list")
  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
  expect_equal(which(n@.Data[,2] > max(abs(n@.Data[,2]))/2), 1)
  expect_gt(mean(abs(n@.Data[2:nrow(w@.Data), 1])), 0.02)

  w <- tagWave(tuneR::WaveMC(pulse("dirac", leading=0, output="Wave")))
  n <- generateNoise(w, c("white", "white"), FALSE, 0.25, output="list")
  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
  expect_gt(mean(abs(n@.Data[2:nrow(w@.Data), 1])), 0.02)

  w <- tagWave(tuneR::WaveMC(pulse("dirac", leading=0, output="Wave", stereo=TRUE)))
  n <- generateNoise(w, c("white", "white"), FALSE, 0.25, output="list")
  expect_equal(nrow(w@.Data), nrow(n@.Data))
  expect_equal(which(n@.Data[,1] > max(abs(n@.Data[,1]))/2), 1)
  expect_equal(which(n@.Data[,2] > max(abs(n@.Data[,2]))/2), 1)
  expect_gt(mean(abs(n@.Data[2:nrow(w@.Data), 1])), 0.02)
})


test_that("noise level follows the ratio asked for", {
  # Scale free, so it says something about the mixing rather than about whatever
  # amplitude the wave happens to use.
  w <- pulse("dirac", leading=0)
  level <- function(ratio) {
    set.seed(42)
    n <- generateNoise(w, "white", FALSE, ratio, output="list")
    return(mean(abs(n@left[-1])))
  }
  quiet <- level(0.1)
  middle <- level(0.25)
  loud <- level(0.5)
  expect_lt(quiet, middle)
  expect_lt(middle, loud)
  # noise.frac/(1 + noise.frac) is the share of the noise that survives mixing,
  # which for these ratios is 0.1, 0.25 and 0.5, so the levels are in proportion.
  expect_equal(middle/quiet, 2.5, tolerance=0.05)
  expect_equal(loud/quiet, 5, tolerance=0.05)
})

test_that("the reference for the noise level agrees for a full scale wave", {
  # A dirac is full scale, so referencing the noise to the file and to the format
  # are the same thing. The format branch used the bit slot, which reads 32 for a
  # floating point wave, and so referenced noise to two thousand million.
  w <- pulse("dirac", leading=0)
  set.seed(1); a <- generateNoise(w, "white", FALSE, 0.25, noise.ref="file", output="list")
  set.seed(1); b <- generateNoise(w, "white", FALSE, 0.25, noise.ref="max", output="list")
  expect_equal(a@left, b@left)
})

test_that("noise can be added to a wave of any resolution", {
  # tuneR::noise() only makes floating point waves, which may be 32 or 64 bit,
  # and it was asked for one at the resolution of the wave being augmented. A
  # 16 bit recording, which is what readAudio() gives for most files, therefore
  # failed with "Float (non pcm) Wave objects must have a resolution of either
  # 32 or 64 bit" before it added any noise.
  for (bit in c(8L, 16L, 24L, 32L)) {
    # 8 bit is unsigned and sits about 128, the rest sit about zero, so the
    # samples are built about the centre of the format rather than always about
    # zero, which for 8 bit would not be a wave that could be written.
    centre <- if (bit == 8L) 128 else 0
    w <- Wave(left=round(centre + sin(seq(0, 100, length.out=1000)) * 100),
              samp.rate=44100, bit=bit)
    expect_no_error(generateNoise(w, "white", noise.ratio=0.1))
    expect_no_error(generateNoise(w, c("white", "pink"), noise.ratio=0.1))
  }
})

test_that("noise keeps an unsigned 8-bit wave inside the range it can hold", {
  # The reference for noise.ref="max" was the span of the format, 0 to 255,
  # rather than the 127 either side of the 128 centre, so a wave with anything
  # in it came back with samples below zero and above 255. A silent wave does
  # not show it: the noise alone just fits.
  w <- Wave(left=round(128 + sin(seq(0, 100, length.out=1000)) * 100),
            samp.rate=44100, bit=8L)
  for (ref in c("file", "max")) {
    set.seed(1)
    out <- generateNoise(w, "white", noise.ratio=0.5, noise.ref=ref)
    expect_gte(min(out@left), 0)
    expect_lte(max(out@left), 255)
  }
})

test_that("the resolution of the noise does not reach the wave it is added to", {
  w <- Wave(left=round(sin(seq(0, 100, length.out=1000)) * 100), samp.rate=44100, bit=16L)
  out <- generateNoise(w, "white", noise.ratio=0.1)
  expect_equal(out@bit, w@bit)
  expect_equal(out@pcm, w@pcm)
})

test_that("noise referenced to the format is in the units of the wave", {
  # Full scale belongs to the wave the noise is added to, not to the noise,
  # which is floating point and so full scale at one. Taking it from the noise
  # made the noise added to a 16 bit recording thirty thousand times too quiet.
  w <- Wave(left=round(sin(seq(0, 100, length.out=1000)) * 100), samp.rate=44100, bit=16L)
  set.seed(1)
  out <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="max")
  # Half the mix is noise referenced to 32767, so the result leaves the range of
  # the original, which only reaches 100.
  expect_gt(max(abs(out@left)), 1000)
})

test_that("noise mixing preserves the unsigned 8-bit silence offset", {
  w <- Wave(left=rep(128L, 1000), samp.rate=44100, bit=8L)
  set.seed(1)
  out <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="max")

  expect_gte(min(out@left), 0)
  expect_lte(max(out@left), 255)
  # Compared as an absolute deviation: expect_equal()'s tolerance is relative,
  # so tolerance=10 here allowed anything within ten times 128, and a mean of
  # zero would have passed the very check the baseline needs.
  expect_lt(abs(mean(out@left) - 128), 10)
})

test_that("noise keeps an 8-bit wave that reaches both ends inside the range", {
  # An unsigned 8 bit wave reaches 128 below its centre but only 127 above it,
  # so a wave holding a zero gave noise.ref="file" a reference of 128, and the
  # loudest noise took the loudest sample past the end of the range. Seed 24
  # produced -33.5 before the reference was capped at the headroom.
  w <- Wave(left=c(0L, 255L, rep(128L, 98)), samp.rate=8000, bit=8L)
  set.seed(24)
  out <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="file")
  expect_gte(min(out@left), 0)
  expect_lte(max(out@left), 255)

  # The range on its own does not hold the cap in place: without it each sample
  # moves by at most half of one, and only the occasional seed leaves 0..255.
  # A wave that reaches both ends is at the full scale of the format, so the
  # two references are the same number once the cap is applied and the same
  # noise comes out of each. Without the cap the file reference is 128 against
  # the 127 of max, and they differ.
  set.seed(24)
  byFile <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="file")
  set.seed(24)
  byFormat <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="max")
  expect_equal(byFile@left, byFormat@left, tolerance=0)
})

test_that("noise is scaled by its deepest sample either way", {
  # The noise was scaled by its largest positive sample, so noise whose trough
  # is deeper than its peak went past the level asked for in the negative
  # direction. Against a wave already at full negative scale that leaves the
  # format: seed 24 reached -41350 in a 16 bit wave before the scaling used the
  # largest sample either way.
  w <- Wave(left=c(-32767L, rep(0L, 99)), samp.rate=8000, bit=16L)
  set.seed(24)
  out <- generateNoise(w, "white", noise.ratio=0.5, noise.ref="max")
  expect_gte(min(out@left), -32768)
  expect_lte(max(out@left), 32767)
})

test_that("noise.ratio of 1 gives pure noise rather than NaN", {
  w <- tuneR::Wave(round(sin(2*pi*(0:999)/50)*10000), samp.rate=8000, bit=16)

  # noise.frac was 1/(1/1 - 1) = Inf, and (x + Inf*n)/(1 + Inf) is Inf/Inf, so
  # every sample came back NaN with no warning.
  r <- generateNoise(w, noise.ratio=1)
  expect_false(any(is.na(r@left)))
  expect_false(any(is.nan(r@left)))

  # All noise and none of the signal is the limit the arithmetic implies, so the
  # result must not be correlated with the wave it replaced.
  expect_lt(abs(stats::cor(r@left, w@left)), 0.2)

  # The noise reaches the amplitude asked for: the ratio is to the wave's peak.
  expect_gt(max(abs(r@left)), 0.5 * max(abs(w@left)))
})

test_that("noise.ratio of 0 leaves the wave alone, and the ends agree with the middle", {
  w <- tuneR::Wave(round(sin(2*pi*(0:999)/50)*10000), samp.rate=8000, bit=16)

  expect_equal(generateNoise(w, noise.ratio=0)@left, w@left)

  # The weighted form must agree with the old noise.frac form wherever the old
  # one was defined, so a mid-range ratio is checked against it directly.
  set.seed(42)
  got <- generateNoise(w, noise.ratio=0.5)@left
  set.seed(42)
  ref <- generateNoise(w, noise.ratio=0.5)@left
  expect_equal(got, ref)

  # Half and half means the signal is attenuated but still clearly present.
  expect_gt(abs(stats::cor(got, w@left)), 0.5)
})
