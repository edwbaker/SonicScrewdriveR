test_that("test windowing using filename", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")

  f1 <- function(start, wave, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(start, wave, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- defaultCluster()

  f1 <- function(start, wave, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1, cl=cl)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(start, wave, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE, cl=cl)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  stopCluster(cl)
})

test_that("test windowing using Wave object", {
  f <- noise("white", duration=48000*5, samp.rate=48000)

  f1 <- function(start, wave, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(start, wave, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- defaultCluster()

  f1 <- function(start, wave, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1, cl=cl)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(start, wave, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE, cl=cl)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  stopCluster(cl)
})

test_that("Overlap gives expected results", {
  f <- noise("white", duration=2000, samp.rate=48000)
  f1 <- function(start, wave, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=500, window.overlap=0, FUN=f1)
  expect_equal(length(ws), 4)

  ws <- windowing(f, window.length=500, window.overlap=-500, FUN=f1)
  expect_equal(length(ws), 2)

  ws <- windowing(f, window.length=500, window.overlap=250, complete.windows=T, FUN=f1)
  expect_equal(length(ws), 7)

  ws <- windowing(f, window.length=500, window.overlap=250, complete.windows=F, FUN=f1)
  expect_equal(length(ws), 8)
})