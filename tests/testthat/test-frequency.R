context("frequency")

test_that("default to air with good known value", {
  expect_equal(frequencySound(343), 1)
})

