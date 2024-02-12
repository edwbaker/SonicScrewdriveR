test_that("default to air with good known value", {
  expect_equal(frequencySound(343), 1)
})

test_that("naturalFrequency gives expected output", {
  expect_lt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, 0.001)
  expect_gt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, -0.001)
  expect_lt(naturalFrequency(L=20,R=0.5) - 0.002950812, 0.001)
  expect_gt(naturalFrequency(L=20,R=0.5) - 0.002950812, -0.001)
})

test_that("resonantFrequency gives expected output", {
  expect_lt(resonantFrequency(1,1) - 0.15915494, 0.001)
  expect_gt(resonantFrequency(1,1) - 0.15915494, -0.001)
})
