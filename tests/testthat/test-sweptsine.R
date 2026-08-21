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
