context("data2Wave")

test_that("only correct inputs are accepted", {
  expect_error(data2Wave(c("cat", "dog")))
  expect_silent(data2Wave(c(1,2,3)))
})

test_that("output is a Wave", {
  expect_silent(validateIsWave(data2Wave(rep_len(0, 100))))
})
