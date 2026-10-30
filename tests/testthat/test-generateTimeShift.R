test_that("generateTimeShift rejects incorrect input", {
  w <- list(
    tuneR::sine(440, samp.rate=44100),
    tuneR::WaveMC(tuneR::sine(440, samp.rate=44100))
  )
  expect_silent(generateTimeShift(w))
  # Both classes must actually be shifted, not silently skipped.
  expect_equal(lengths(generateTimeShift(w)), c(2, 2))
  expect_error(generateTimeShift(w, type="pitcher plant"), "Unknown value for type.")
  expect_error(generateTimeShift(w, where="pitcher"), "Unknown value for where.")

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

test_that("generateTimeShift shifts WaveMC objects", {
  w <- tuneR::sine(440, samp.rate=1000, duration=3000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left * 0.5), samp.rate=1000, bit=16)
  colnames(mc@.Data) <- c("A", "B")

  for (type in c("silent", "rotate")) {
    l <- generateTimeShift(mc, type=type)
    expect_length(l, 2)
    expect_s4_class(l[[1]], "WaveMC")
    expect_equal(nrow(l[[1]]@.Data), length(mc))
    expect_equal(colnames(l[[1]]@.Data), c("A", "B"))
  }

  expect_length(generateTimeShift(mc, type="silent", where="both"), 4)
})

test_that("rotating a WaveMC moves the audio without losing any", {
  w <- tuneR::sine(440, samp.rate=1000, duration=3000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left * 0.5), samp.rate=1000, bit=16)

  rotated <- generateTimeShift(mc, type="rotate", amount=1)[[1]]
  expect_equal(rotated@.Data[1:1000, 1], mc@.Data[2001:3000, 1])
  expect_equal(sort(rotated@.Data[,1]), sort(mc@.Data[,1]))
})

test_that("inserting silence into a WaveMC silences every channel", {
  w <- tuneR::sine(440, samp.rate=1000, duration=3000)
  mc <- tuneR::WaveMC(cbind(w@left, w@left * 0.5), samp.rate=1000, bit=16)

  shifted <- generateTimeShift(mc, type="silent", amount=1)[[1]]
  expect_true(all(shifted@.Data[1:1000, ] == 0))
  expect_equal(shifted@.Data[1001:3000, 1], mc@.Data[1:2000, 1])
})

test_that("a shift that rounds to no samples returns the wave unchanged", {
  # tuneR::silence() refuses a duration of zero, so a shift shorter than half a
  # sample aborted for a Wave while a WaveMC, whose silence is built as a
  # matrix, went through. amount=c(0, 1, 2) - the unshifted wave alongside two
  # shifts - therefore failed for one class and not the other.
  w <- tuneR::Wave(as.numeric(1:10), samp.rate=8000, bit=32, pcm=FALSE)

  expect_silent(generateTimeShift(w, type="silent", amount=1e-6))
  expect_equal(generateTimeShift(w, type="silent", amount=0)[[1]]@left, as.numeric(1:10))
  expect_equal(generateTimeShift(w, type="silent", amount=1e-6)[[1]]@left, as.numeric(1:10))
  expect_equal(generateTimeShift(w, type="rotate", amount=0)[[1]]@left, as.numeric(1:10))

  # The whole point of the report: a zero alongside real shifts no longer
  # aborts the call, and the real shifts still happen.
  r <- generateTimeShift(w, type="silent", amount=c(0, 0.001), where="start")
  expect_equal(r[[1]]@left, as.numeric(1:10))
  expect_equal(r[[2]]@left, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2))

  # A WaveMC behaves the same way, as it did before.
  mc <- tuneR::WaveMC(cbind(1:10, 11:20), samp.rate=8000, bit=32, pcm=FALSE)
  expect_equal(nrow(generateTimeShift(mc, type="silent", amount=0)[[1]]@.Data), 10L)
})
