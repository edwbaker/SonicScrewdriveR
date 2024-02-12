test_that("only correct inputs are accepted", {
  expect_error(data2Wave(c("cat", "dog")))
  expect_no_condition(data2Wave(c(1,2,3)))
})

test_that("output is a Wave", {
  expect_no_condition(validateIsWave(data2Wave(rep_len(0, 100))))
})

test_that("DC offset removal works", {
  # Generate sequence with offset
  w <- tuneR::sine(440)
  d <- (w@left/max(w@left)) + 1

  expect_lt(mean(data2Wave(d, remove.offset = TRUE)@left), 0.001)
  expect_lt((mean(data2Wave(d, remove.offset = FALSE, normalise=F)@left-1))^2, 0.001)
})

test_that("Normalisation works", {
  # Generate sequence > 1
  w <- tuneR::sine(440)
  d <- w@left * 2

  expect_lte(max(data2Wave(d, normalise = TRUE)@left), 1)
  expect_gt(max(data2Wave(d, normalise = FALSE)@left), 1)

  # Generate sequence < 1
  d <- w@left / 2

  expect_gte(max(data2Wave(d, normalise = TRUE)@left), 0.99)
  expect_lte(max(data2Wave(d, normalise = FALSE)@left), 0.5)
})
