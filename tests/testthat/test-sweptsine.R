test_that("Duration of swept sine", {
  expect_equal(length(sweptsine(sweep.time=1, time.unit="seconds", samp.rate=44100)@left), 44100)
  expect_equal(length(sweptsine(sweep.time=44100, time.unit="samples", samp.rate=44100)@left), 44100)
})

test_that("Reject incorrect input", {
  expect_error(sweptsine(time.unit="dogs", "time.unit must be one of 'seconds' or 'samples'"))
  expect_error(sweptsine(output="cats"), "output must be one of 'wave' or 'vector'")
  expect_error(sweptsine(10,0), "sweptsine: f1 must be greater than f0")
})

test_that("Output in correct format", {
  expect_equal(class(sweptsine(output="vector")), "numeric")
  expect_no_condition(validateIsWave(sweptsine(output="wave")))
})

test_that("Initial value is zero", {
  expect_equal(sweptsine(0,100)@left[1], 0)
  expect_equal(sweptsine(1, 10e3)@left[1], 0)
})
