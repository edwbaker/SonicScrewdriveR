test_that("Duration of swept sine", {
  expect_equal(length(sweptsine(sweep.time=1, time.unit="seconds", samp.rate=44100)@left), 44100)
  expect_equal(length(sweptsine(sweep.time=44100, time.unit="samples", samp.rate=44100)@left), 44100)
})

test_that("Reject incorrect input", {
  expect_error(sweptsine(time.unit="dogs", "time.unit must be one of 'seconds' or 'samples'"))
  expect_error(sweptsine(output="cats"), "output must be one of 'wave' or 'vector'")
})

test_that("Output in correct format", {
  expect_equal(class(sweptsine(output="vector")), "numeric")
  expect_no_condition(validateIsWave(sweptsine(output="wave")))
})
