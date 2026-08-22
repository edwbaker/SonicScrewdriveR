test_that("pulse amplitude suits the unit asked for", {
  # tuneR::silence() resolves bit=1 to a Wave whose bit slot reads 32, so taking
  # the amplitude from that slot gave values far outside the allowed range and
  # writeWave() refused them.
  for (args in list(list(), list(bit=16, pcm=TRUE), list(bit=8),
                    list(bit=32, pcm=TRUE), list(bit=64))) {
    p <- suppressWarnings(do.call(pulse, args))
    f <- tempfile(fileext=".wav")
    expect_silent(tuneR::writeWave(p, f))
    expect_true(file.exists(f))
  }
})

test_that("pulse uses the full range of a floating point wave", {
  expect_equal(max(pulse()@left), 1)
  expect_equal(min(pulse(invert=TRUE)@left), -1)
})

test_that("pulse stays inside the range of a PCM wave", {
  p <- suppressWarnings(pulse(bit=16, pcm=TRUE))
  expect_equal(max(p@left), 2^15 - 1)
  expect_gte(min(suppressWarnings(pulse(bit=16, pcm=TRUE, invert=TRUE))@left), -2^15)
})

test_that("a square pulse of no length writes nothing", {
  # (leading+1):(leading+0) counted backwards and wrote two samples.
  expect_equal(sum(pulse(type="square", pulse.length=0)@left != 0), 0)
  expect_equal(sum(pulse(type="square", pulse.length=5)@left != 0), 5)
})
