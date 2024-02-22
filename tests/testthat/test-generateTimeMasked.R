test_that("generateTimeMasked rejects incorrect method", {
  w <- tuneR::sine(440)
  expect_error(
    generateTimeMasked(w, method="capybara"),
    "Unknown method parameter to generateTimeMasked: capybara"
  )
  expect_silent(generateTimeMasked(w, method="squarewave"))
})
