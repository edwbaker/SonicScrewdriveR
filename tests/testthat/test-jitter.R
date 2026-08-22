# Signals built from whole numbers of samples per period, so that a correct
# measurement returns exactly the period variation that was put in.
periodic <- function(periods, shape=function(p) sin(2*pi*(0:(p-1))/p), samp.rate=44100) {
  x <- unlist(lapply(periods, shape))
  tuneR::Wave(round(x * 2^13), samp.rate=samp.rate, bit=16)
}

# Asymmetric shapes: each period is one cycle, but the two halves differ in length.
offset_shape <- function(p) sin(2*pi*(0:(p-1))/p) + 0.3
harmonic_shape <- function(p) sin(2*pi*(0:(p-1))/p) + 0.5*sin(4*pi*(0:(p-1))/p + 1)

test_that("a waveform of constant period has no jitter", {
  for (shape in list(NULL, offset_shape, harmonic_shape)) {
    w <- if (is.null(shape)) periodic(rep.int(220, 200)) else periodic(rep.int(220, 200), shape)
    expect_equal(jitter(w), 0, tolerance=1e-6)
    expect_equal(jitter(w, method="relative"), 0, tolerance=1e-6)
  }
})

test_that("an asymmetric waveform of constant period is not reported as jittered", {
  # The two halves of these waveforms differ in length, so measuring every zero
  # crossing rather than every period finds a large difference between successive
  # measurements even though the period never changes.
  for (shape in list(offset_shape, harmonic_shape)) {
    w <- periodic(rep.int(220, 200), shape)

    half.cycles <- diff(zerocross(w))
    expect_gt(sum(abs(diff(half.cycles))) / (length(half.cycles)-1), 40)

    expect_equal(jitter(w), 0, tolerance=1e-6)
  }
})

test_that("absolute jitter is the mean difference between period lengths", {
  # Periods alternating by 2 samples, so every consecutive difference is 2.
  expect_equal(jitter(periodic(rep(c(220, 222), length.out=200))), 2, tolerance=1e-6)
  # Periods alternating by 6 samples.
  expect_equal(jitter(periodic(rep(c(220, 226), length.out=200))), 6, tolerance=1e-6)
})

test_that("relative jitter scales absolute jitter by the mean period", {
  w <- periodic(rep(c(220, 222), length.out=200))

  expect_equal(jitter(w, method="relative"), jitter(w) / 221, tolerance=1e-3)
})

test_that("jitter returns NA when there are too few periods", {
  w <- tuneR::Wave(rep.int(1, 10), samp.rate=44100, bit=16)

  expect_true(is.na(jitter(w)))
  expect_true(is.na(jitter(w, method="relative")))
})

test_that("jitter rejects incorrect input", {
  expect_error(jitter("not a wave"), "Expecting a Wave object")
  expect_error(
    jitter(periodic(rep.int(220, 10)), method="sonic"),
    "Unknown method for jitter: sonic"
  )
})
