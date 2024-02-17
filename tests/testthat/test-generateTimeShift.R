test_that("generateTimeShift rejects incorrect input", {
  w <- list(
    tuneR::sine(440, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, samp.rate=44100))
  )
  expect_silent(generateTimeShift(w))
  expect_error(generateTimeShift(w, type="pitcher plant"), "Unknown value for type.")
  expect_error(generateTimeShift(w, output="leaf insect"), "Unknown value for output.")

  w <- list(
    tuneR::sine(440, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, samp.rate=44100)),
    1
  )
  expect_error(generateTimeShift(w), "All elements of wave must be Wave-like objects.")

  expect_error(generateTimeShift(1), "All elements of wave must be Wave-like objects.")


})

test_that("generateTimeshift gives correct format output", {
  p <- pulse(leading=0, duration=44100*3)
  l <- generateTimeShift(p, type="silent")
  expect_length(l,2)
  expect_type(l, "list")
  expect_s4_class(l[[1]], "Wave")
  expect_equal(length(p@left), length(l[[1]]@left))

  p <- pulse(leading=0, duration=44100*3)
  l <- generateTimeShift(p, type="rotate")
  expect_length(l,2)
  expect_type(l, "list")
  expect_s4_class(l[[1]], "Wave")
  expect_equal(length(p@left), length(l[[1]]@left))

  p <- pulse(leading=0, duration=44100*3)
  l <- generateTimeShift(p, type="silent", where="end")
  expect_length(l,2)
  expect_type(l, "list")
  expect_s4_class(l[[1]], "Wave")
  expect_equal(length(p@left), length(l[[1]]@left))

  p <- pulse(leading=0, duration=44100*3)
  l <- generateTimeShift(p, type="silent", where="both")
  expect_length(l,4)
  expect_type(l, "list")
  expect_s4_class(l[[1]], "Wave")
  expect_equal(length(p@left), length(l[[1]]@left))
})
