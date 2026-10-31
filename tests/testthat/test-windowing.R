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

test_that("bind.wave fills the last gap of a file whose length is no whole number of microseconds", {
  skip_if_not_installed("av")
  # The av package gives the duration of 44101 samples at 44100 Hz as
  # 1.000023 seconds, or 44101.0143 samples, and the gap after the last window
  # is read up to that count, which readAudio() refuses as a fraction of a
  # sample.
  w <- tuneR::Wave(
    left=as.integer(round(sin(seq_len(44101) / 7) * 1000)), samp.rate=44100, bit=16
  )
  idfun <- function(wave, start, window.length) wave
  for (ext in c(".wav", ".flac")) {
    f <- withr::local_tempfile(fileext=ext)
    writeAudio(w, f)
    ws <- windowing(f, window.length=10000, window.overlap=-10000, FUN=idfun, bind.wave=TRUE)
    expect_equal(ws@left, w@left, info=ext)
  }
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

test_that("complete.windows=FALSE hands FUN the shorter final window, not NA padding", {
  w <- tuneR::Wave(rep(1, 1000), samp.rate=48000, bit=16)
  means <- windowing(w, window.length=300, window.overlap=0, complete.windows=FALSE,
                     FUN=function(wave, start, window.length) mean(wave@left))
  expect_equal(unlist(means), c(1, 1, 1, 1))

  lengths <- windowing(w, window.length=300, window.overlap=0, complete.windows=FALSE,
                       FUN=function(wave, start, window.length) length(wave))
  expect_equal(unlist(lengths), c(300, 300, 300, 100))
})

# The bytes of a wave file cut short `keep` bytes into its audio, as a flat
# battery, a full card or a copy that did not finish leaves a recording.
cutShort <- function(bytes, keep) {
  data <- .wavFindChunk(.wavChunks(bytes)$chunks, "data")
  return(bytes[seq_len(data$start - 1 + keep)])
}

waveBytes <- function(wave) {
  f <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(wave, f)
  return(readBin(f, "raw", n=file.size(f)))
}

# Reading a file that was cut short may say so, which is not what these tests
# are about, so only that warning is passed over.
heldWindows <- function(...) {
  return(suppressWarnings(windowing(...), classes="truncatedAudio"))
}

windowLengths <- function(wave, start, window.length) length(wave)

test_that("A file cut short is windowed over the audio it holds", {
  samples <- round(sin(seq_len(8000) / 5) * 10000)
  w <- tuneR::Wave(samples, samp.rate=8000, bit=16)
  # The header declares 8000 samples, of which the file holds 4500.
  f <- mdFile(cutShort(waveBytes(w), 9000))
  held <- tuneR::Wave(samples[1:4500], samp.rate=8000, bit=16)

  expect_equal(unlist(heldWindows(f, window.length=1000, FUN=windowLengths)), rep(1000, 4))
  expect_equal(
    unlist(heldWindows(f, window.length=1000, complete.windows=FALSE, FUN=windowLengths)),
    c(rep(1000, 4), 500)
  )
  # Ending at a window boundary leaves no shorter window to add.
  whole <- mdFile(cutShort(waveBytes(w), 10000))
  expect_equal(
    unlist(heldWindows(whole, window.length=1000, complete.windows=FALSE, FUN=windowLengths)),
    rep(1000, 5)
  )

  # Each window is what the Wave of the audio held gives.
  samplesOf <- function(wave, start, window.length) wave@left
  for (complete in c(TRUE, FALSE)) {
    expect_equal(
      heldWindows(f, window.length=1000, window.overlap=250, complete.windows=complete, FUN=samplesOf),
      windowing(held, window.length=1000, window.overlap=250, complete.windows=complete, FUN=samplesOf),
      info=complete
    )
  }
})

test_that("bind.wave fills the gaps between windows of a file cut short to its end", {
  samples <- round(sin(seq_len(8000) / 5) * 10000)
  w <- tuneR::Wave(samples, samp.rate=8000, bit=16)
  idfun <- function(wave, start, window.length) wave

  # 4500 samples end within the gap after the second window when only complete
  # windows are analysed, and within a third, shorter window otherwise; 5000
  # end with a window.
  for (keep in c(4500, 5000)) {
    f <- mdFile(cutShort(waveBytes(w), 2 * keep))
    for (complete in c(TRUE, FALSE)) {
      ws <- heldWindows(f, window.length=1000, window.overlap=-1000, bind.wave=TRUE,
                        complete.windows=complete, FUN=idfun)
      expect_equal(ws@left, samples[1:keep], info=paste(keep, complete))
    }
  }
})

test_that("An RF64 file cut short is windowed over the audio it holds", {
  skip_if_not_installed("av")
  f <- mdFile(cutShort(mdRf64Wav(1:8000, after=raw(0)), 9000))

  expect_equal(unlist(heldWindows(f, window.length=1000, FUN=windowLengths)), rep(1000, 4))
  expect_equal(
    unlist(heldWindows(f, window.length=1000, complete.windows=FALSE, FUN=windowLengths)),
    c(rep(1000, 4), 500)
  )
})

test_that("A file whose audio size was never written is windowed over the audio recovered", {
  samples <- round(sin(seq_len(8000) / 5) * 10000)
  bytes <- waveBytes(tuneR::Wave(samples, samp.rate=8000, bit=16))
  data <- .wavFindChunk(.wavChunks(bytes)$chunks, "data")
  # The sizes a recorder writes as it starts, and never comes back to.
  bytes[(data$start - 4):(data$start - 1)] <- .uintToRaw(0, 4L)
  bytes[5:8] <- .uintToRaw(data$start - 9, 4L)
  f <- mdFile(bytes)

  ws <- suppressWarnings(
    windowing(f, window.length=1000, FUN=windowLengths), classes="unwrittenAudioSize"
  )
  expect_equal(unlist(ws), rep(1000, 8))
  ws <- suppressWarnings(
    windowing(f, window.length=1000, window.overlap=-1000, bind.wave=TRUE,
              FUN=function(wave, start, window.length) wave),
    classes="unwrittenAudioSize"
  )
  expect_equal(ws@left, samples)
})

test_that("Audio holding no samples has no windows", {
  w <- tuneR::Wave(round(sin(seq_len(8000) / 5) * 10000), samp.rate=8000, bit=16)
  # Cut short before the first byte of audio, and part way through the first
  # sample.
  for (keep in c(0, 1)) {
    f <- mdFile(cutShort(waveBytes(w), keep))
    for (complete in c(TRUE, FALSE)) {
      expect_equal(
        heldWindows(f, window.length=1000, complete.windows=complete, FUN=windowLengths),
        list(), info=paste(keep, complete)
      )
    }
  }

  empty <- tuneR::Wave(numeric(0), samp.rate=8000, bit=16)
  expect_equal(windowing(empty, window.length=1000, FUN=windowLengths), list())
  expect_equal(windowing(empty, window.length=1000, complete.windows=FALSE, FUN=windowLengths), list())
  expect_equal(
    windowing(empty, window.length=1000, bind.wave=TRUE, FUN=function(wave, start, window.length) wave),
    list()
  )
})

test_that("A file read through the av package is windowed over every sample it holds", {
  skip_if_not_installed("av")
  # The av package gives the duration of a FLAC file to the microsecond, so
  # these lengths come back a fraction of a sample short or long.
  short <- withr::local_tempfile(fileext=".flac")
  writeAudio(tuneR::Wave(round(runif(3000, -1000, 1000)), samp.rate=44100, bit=16), short)
  expect_equal(unlist(windowing(short, window.length=3000, FUN=windowLengths)), 3000)
  expect_equal(unlist(windowing(short, window.length=1000, FUN=windowLengths)), rep(1000, 3))

  long <- withr::local_tempfile(fileext=".flac")
  writeAudio(tuneR::Wave(round(runif(12345, -1000, 1000)), samp.rate=48000, bit=16), long)
  expect_equal(
    unlist(windowing(long, window.length=12345, complete.windows=FALSE, FUN=windowLengths)),
    12345
  )

  # A file that is not there is left to the av package to say so.
  missing <- file.path(withr::local_tempdir(), "missing.wav")
  expect_no_warning(expect_error(windowing(missing, FUN=windowLengths)))

  # The FLAC and MP3 files shipped with the package are windowed as the wave
  # file they were made from is.
  for (x in c("AUDIOMOTH.flac", "AUDIOMOTH.mp3")) {
    f <- system.file("extdata", x, package="sonicscrewdriver")
    expect_equal(unlist(windowing(f, window.length=10000, FUN=windowLengths)), rep(10000, 24), info=x)
  }
})

test_that("an overlap at or above the window length is refused by name", {
  w <- tuneR::Wave(as.numeric(1:1000), samp.rate=8000, bit=32, pcm=FALSE)

  # The hop is window.length - window.overlap, so these left nothing to advance
  # by and failed inside seq_len() with "argument must be coercible to
  # non-negative integer", which names neither argument.
  expect_error(
    windowing(w, window.length=1000, window.overlap=1000, FUN=function(...) 1),
    "In windowing window.overlap must be less than window.length"
  )
  expect_error(
    windowing(w, window.length=1000, window.overlap=1200, FUN=function(...) 1),
    "In windowing window.overlap must be less than window.length"
  )

  # An overlap just below the window length is a hop of one sample, which is
  # extreme but possible, and is still allowed.
  expect_silent(windowing(w, window.length=10, window.overlap=9, FUN=function(...) 1))
})
