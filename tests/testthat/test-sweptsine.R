test_that("Duration of swept sine", {
  expect_equal(length(sweptsine(sweep.time=1, time.unit="seconds", samp.rate=44100)@left), 44100)
  expect_equal(length(sweptsine(sweep.time=44100, time.unit="samples", samp.rate=44100)@left), 44100)
})

test_that("Reject incorrect input", {
  expect_error(sweptsine(1, 10, time.unit="dogs", "time.unit must be one of 'seconds' or 'samples'"))
  expect_error(sweptsine(1, 10, output="cats"), "output must be one of 'wave' or 'vector'")
  expect_error(sweptsine(10,1), "sweptsine: f1 must be greater than f0")
  expect_error(sweptsine(1,10, mode="hare"), "sweptsine: mode must be one of 'linear' or 'log'")
  expect_error(sweptsine(0,10, mode="log"), "sweptsine: f0 must be greater than zero in logarithmic mode")
})

test_that("Output in correct format", {
  expect_equal(class(sweptsine(output="vector")), "numeric")
  expect_no_condition(validateIsWave(sweptsine(output="wave")))
})

test_that("Initial value is zero", {
  expect_equal(sweptsine(0,100)@left[1], 0)
  expect_equal(sweptsine(1, 10e3)@left[1], 0)
  expect_equal(sweptsine(1, 10e3, mode="log")@left[1], 0)
})

# Mean frequency of a vector, from the rate of zero crossings.
zcFreq <- function(x, samp.rate) {
  sum(diff(sign(x)) != 0) / 2 / (length(x) / samp.rate)
}

test_that("Linear sweep covers the requested frequencies", {
  sr <- 44100
  win <- 0.1 * sr
  w <- sweptsine(100, 2500, sweep.time=1, samp.rate=sr, output="vector")

  # A window of a linear sweep has the mean frequency of its centre.
  expect_equal(zcFreq(head(w, win), sr), 220, tolerance=0.01)
  expect_equal(zcFreq(tail(w, win), sr), 2380, tolerance=0.01)
})

test_that("Logarithmic sweep covers the requested frequencies", {
  sr <- 44100
  win <- 0.1 * sr
  w <- sweptsine(100, 2500, mode="log", sweep.time=1, samp.rate=sr, output="vector")
  R <- log(2500/100)

  expect_equal(zcFreq(head(w, win), sr), 100 * exp(R * 0.05), tolerance=0.03)
  expect_equal(zcFreq(tail(w, win), sr), 100 * exp(R * 0.95), tolerance=0.03)
})

test_that("Sweeps are identical whether sweep.time is in seconds or samples", {
  sr <- 44100
  for (mode in c("linear", "log")) {
    expect_equal(
      sweptsine(100, 2500, mode=mode, sweep.time=1, samp.rate=sr, output="vector"),
      sweptsine(100, 2500, mode=mode, sweep.time=sr, time.unit="samples",
                samp.rate=sr, output="vector")
    )
  }
})

test_that("a fractional count of samples is refused rather than rounded", {
  # seq(length=) rounds, so this gave a one-sample "sweep" with no warning.
  expect_error(
    sweptsine(1000, 4000, sweep.time=0.5, time.unit="samples", output="vector"),
    "must be a whole number of samples"
  )
  expect_error(
    sweptsine(1000, 4000, sweep.time=100.5, time.unit="samples", output="vector"),
    "must be a whole number of samples"
  )
  # A whole number of samples is still accepted.
  expect_equal(
    length(sweptsine(1000, 4000, sweep.time=256, time.unit="samples", output="vector")),
    256
  )
})

test_that("the sweep is generated over the duration its phase is integrated over", {
  # vector_length was left unrounded while seq() rounded it, so the wave was a
  # different length from the sweep.duration that set the phase, and the
  # frequency at the last sample was not quite f1.
  x <- sweptsine(1000, 4000, sweep.time=1.00005, samp.rate=44100, output="vector")
  expect_equal(length(x), round(44100 * 1.00005))

  # The instantaneous frequency at the last sample is f1, which is only true if
  # the duration and the generated length agree. Measured from the final zero
  # crossing interval of the linear sweep.
  f <- 44100
  n <- length(x)
  # Half-period at the end, in samples, gives the final frequency.
  tail.crossings <- which(diff(sign(x[(n-200):n])) != 0)
  half.period <- diff(tail.crossings)
  expect_equal(f / (2 * mean(half.period)), 4000, tolerance = 0.01)
})
