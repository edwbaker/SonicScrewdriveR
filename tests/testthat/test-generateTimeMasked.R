test_that("generateTimeMasked rejects incorrect method", {
  w <- tuneR::sine(440)
  expect_error(
    generateTimeMasked(w, method="capybara"),
    "Unknown method parameter to generateTimeMasked: capybara"
  )
  w <- list(w,w,w,w)
  expect_silent(generateTimeMasked(w, method="squarewave"))
})

test_that("random method works as expected", {
  w <- data2Wave(rep_len(1, 100), normalise = F, remove.offset = F)
  wn <- generateTimeMasked(w, method="random", dutyCycle=0.95)
  expect_equal(length(wn), length(w))
  expect_equal(sum(wn@left), 95)

  wn <- generateTimeMasked(w, method="random", dutyCycle=0.15)
  expect_equal(length(wn), length(w))
  expect_equal(sum(wn@left), 15)
})

test_that("squarewave method works as expected", {
  w <- data2Wave(rep_len(1, 1000), normalise = F, remove.offset = F)
  wn <- generateTimeMasked(w, method="squarewave", dutyCycle=0.95)
  expect_equal(length(wn), length(w))
  expect_equal(sum(wn@left), 950)

  w <- tuneR::WaveMC(stereo(w,w))
  wn <- generateTimeMasked(w, method="squarewave", dutyCycle=0.95)
  expect_equal(nrow(wn), nrow(w))
  expect_equal(sum(wn@.Data[,1]), 950)
  expect_equal(sum(wn@.Data[,2]), 950)
})
