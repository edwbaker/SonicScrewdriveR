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

test_that("generateNoise preserves PCM settings from audio read through av", {
  skip_if_not_installed("av")

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))
  expect_silent(generateNoise(w, "white", FALSE, 0.1, output="list"))
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
