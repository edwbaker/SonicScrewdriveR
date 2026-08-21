test_that("test windowing using filename", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(wave, start, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }

  cl <- makeForkCluster(2, outfile="")

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1, cl=cl)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(wave, start, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE, cl=cl)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  parallel::stopCluster(cl)
})

test_that("test windowing using Wave object", {
  f <- noise("white", duration=48000*5, samp.rate=48000)

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(wave, start, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }

  cl <- makeForkCluster(2, outfile="")

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1, cl=cl)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  f2 <- function(wave, start, window.length) {
    return(tuneR::silence(1))
  }
  ws <- windowing(f, window.length=10000, FUN=f2, bind.wave=TRUE, cl=cl)
  expect_silent(validateIsWave(ws))
  expect_equal(length(ws@left), 24)

  parallel::stopCluster(cl)
})

test_that("Overlap gives expected results", {
  f <- noise("white", duration=2000, samp.rate=48000)
  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=500, window.overlap=0, FUN=f1)
  expect_equal(length(ws), 4)

  ws <- windowing(f, window.length=500, window.overlap=-500, FUN=f1, bind.wave=FALSE)
  expect_equal(length(ws), 2)

  ws <- windowing(f, window.length=500, window.overlap=250, complete.windows=T, FUN=f1)
  expect_equal(length(ws), 7)

  ws <- windowing(f, window.length=500, window.overlap=250, complete.windows=F, FUN=f1)
  expect_equal(length(ws), 8)
})

test_that("windowing() rejects incorrect input", {
  expect_error(windowing(123, 4, {print("Oh")}), "Expecting a Wave object")
  w <- sine(440)
  f1 <- function(wave, start, window.length) {
    return(1)
  }
  expect_error(windowing(w, FUN=f1, window.overlap=1, bind.wave=TRUE), "Cannot bind waves with positive overlap.")
})

test_that("works as expected without pbapply installed single core", {
  local_mocked_bindings(
    package.installed = function(...) { return(FALSE)}
  )
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)
})

test_that("works as expected without pbapply installed multi-core", {
  local_mocked_bindings(
    package.installed = function(...) { return(FALSE)}
  )
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- makeForkCluster(2, outfile="")

  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")

  f1 <- function(wave, start, window.length) {
    return(1)
  }
  ws <- windowing(f, window.length=10000, FUN=f1, cl=cl)
  expect_equal(length(ws), 24)
  expect_equal(sum(as.numeric(ws)), 24)

  parallel::stopCluster(cl)
})



test_that("bind wave works as expected", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- tuneR::readWave(f)
  f1 <- function(wave, start, window.length) {
    return(wave)
  }
  ws <- windowing(w, window.length=10000, FUN=f1, bind.wave=TRUE)
  expect_equal(w@left, ws@left)

  f2 <- function(wave, start, window.length) {
    return(wave)
  }

  ws2 <- windowing(f, window.length=10000, window.overlap=-10000, FUN=f2, bind.wave=TRUE)
  expect_equal(w@left, ws2@left)
})

test_that("bind.wave concatenates windows in order", {
  set.seed(21)
  w <- tuneR::Wave(round(runif(5000, -2^13, 2^13)), samp.rate=8000, bit=16)
  idfun <- function(wave, start, window.length) wave

  ws <- windowing(w, window.length=500, window.overlap=0, FUN=idfun, bind.wave=TRUE)
  expect_equal(ws@left, w@left)
  expect_equal(ws@samp.rate, w@samp.rate)
  expect_equal(ws@bit, w@bit)
})

test_that("bind.wave works when only one window is analysed", {
  w <- noise("white", duration=1000, samp.rate=48000)
  idfun <- function(wave, start, window.length) wave

  ws <- windowing(w, window.length=900, window.overlap=0, FUN=idfun, bind.wave=TRUE)
  expect_equal(length(ws@left), 900)
})

test_that("bind.wave fills the gaps between windows from a Wave object", {
  w <- noise("white", duration=2000, samp.rate=48000)
  idfun <- function(wave, start, window.length) wave

  ws <- windowing(w, window.length=500, window.overlap=-500, FUN=idfun, bind.wave=TRUE)
  expect_equal(ws@left, w@left)
})

test_that("bind.wave gives the same result from a Wave object and a filename", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- tuneR::readWave(f)
  idfun <- function(wave, start, window.length) wave

  from.wave <- windowing(w, window.length=10000, window.overlap=-10000, FUN=idfun, bind.wave=TRUE)
  from.file <- windowing(f, window.length=10000, window.overlap=-10000, FUN=idfun, bind.wave=TRUE)
  expect_equal(from.wave@left, w@left)
  expect_equal(from.wave@left, from.file@left)
})

test_that("a wave of exactly one window length is analysed once", {
  w <- noise("white", duration=2000, samp.rate=48000)
  idfun <- function(wave, start, window.length) wave

  ws <- windowing(w, window.length=2000, window.overlap=0, FUN=idfun, bind.wave=TRUE)
  expect_equal(ws@left, w@left)

  counts <- windowing(w, window.length=2000, window.overlap=0,
                      FUN=function(wave, start, window.length) 1)
  expect_equal(length(counts), 1)
})
