context("defaultCluster")
library(parallel)

test_that("correct number of cores", {
  cl <- defaultCluster()
  expect_equal(max(1, detectCores() - 1), length(cl))
  stopCluster(cl)
  cl <- defaultCluster(fork=FALSE)
  expect_equal(max(1, detectCores() - 1), length(cl))
  stopCluster(cl)
})