context("exportPeaks")

test_that("parameters are of correct type", {
  expect_error(exportPeaks("string"), "exportPeaks expects a Wave object")
  expect_error(exportPeaks(1), "exportPeaks expects a Wave object")
  expect_silent(exportPeaks(sine(1000)))
  
  expect_error(suppressWarnings(exportPeaks(sine(1000),n="string")), "n must be an integer")
  expect_error(exportPeaks(sine(1000),n=1.5), "n must be an integer")
  expect_silent(exportPeaks(sine(1000),n=1))
  
  expect_error(exportPeaks(sine(1000),scale="string"))
  expect_silent(exportPeaks(sine(1000),scale=1.5))
  expect_silent(exportPeaks(sine(1000),scale=1))

})

test_that("return value is a vector", {
  expect_equal(is.vector(exportPeaks(sine(1000))), TRUE)
  expect_equal(is.list(exportPeaks(sine(1000))), FALSE)
  expect_equal(is.numeric(exportPeaks(sine(1000))), TRUE)
})

test_that("scaling works", {
  expect_equal(max(exportPeaks(Wave(1:100, samp.rate=1,bit=1), scale=1)), 1)
  expect_equal(max(exportPeaks(Wave(1:100, samp.rate=1,bit=1), scale=200)), 200)
})

test_that("sanity check on meanPeaks", {
  expect_equal(meanPeaks(1, Wave(rep_len(0.5, 100), samp.rate=1,bit=1), window.length=100), 0.5)
})