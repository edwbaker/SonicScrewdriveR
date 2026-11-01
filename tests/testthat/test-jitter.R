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

test_that("jitter measures every channel", {
  # A stereo wave read only its left channel, so the jitter of the right one was
  # never reported, and a WaveMC has no left slot to read at all.
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*250*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1100*(1:n)/sr) * 20000), samp.rate=sr, bit=16)

  left <- jitter(L)
  right <- jitter(R)
  expect_false(isTRUE(all.equal(left, right)))
  expect_false(isTRUE(all.equal(jitter(tuneR::stereo(L, R)), left)))

  expect_equal(jitter(tuneR::stereo(L, R)), list(list(left), list(right)))
  expect_equal(
    jitter(tuneR::WaveMC(cbind(L@left, R@left, L@left), samp.rate=sr, bit=16)),
    list(list(left), list(right), list(left))
  )
})

test_that("jitter passes its method to each channel", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*250*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1100*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  st <- tuneR::stereo(L, R)

  expect_equal(
    jitter(st, method="relative"),
    list(list(jitter(L, method="relative")), list(jitter(R, method="relative")))
  )
  expect_false(isTRUE(all.equal(jitter(st, method="relative"), jitter(st, method="absolute"))))

  # An unknown method is reported once, before the channels are split, rather
  # than once per channel.
  expect_error(jitter(st, method="not-a-method"), "Unknown method for jitter: not-a-method")
})
