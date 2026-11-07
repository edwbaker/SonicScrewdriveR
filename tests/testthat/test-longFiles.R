# A WAV file of noise in any of the formats readWave() reads
.longWav <- function(path, bits=16, channels=1, frames=6000, rate=8000, float=FALSE, seed=1) {
  set.seed(seed)
  m <- if (float) {
    matrix(stats::runif(frames * channels, -1, 1), frames, channels)
  } else {
    m <- matrix(as.integer(round(stats::runif(frames * channels, -1, 1) * (2^(bits - 1) - 1))),
                frames, channels)
    if (bits == 8) m + 128L else m
  }
  w <- if (channels == 1) {
    tuneR::Wave(left=m[, 1], samp.rate=rate, bit=bits, pcm=!float)
  } else if (channels == 2) {
    tuneR::Wave(left=m[, 1], right=m[, 2], samp.rate=rate, bit=bits, pcm=!float)
  } else {
    colnames(m) <- tuneR::MCnames$name[seq_len(channels)]
    tuneR::WaveMC(data=m, samp.rate=rate, bit=bits, pcm=!float)
  }
  tuneR::writeWave(w, path, extensible=channels > 2)
  return(path)
}

# The formats a long file can be read a window at a time in
.longFormats <- list(
  list(name="16-bit mono", bits=16, channels=1),
  list(name="16-bit stereo", bits=16, channels=2),
  list(name="8-bit mono", bits=8, channels=1),
  list(name="24-bit mono", bits=24, channels=1),
  list(name="32-bit mono", bits=32, channels=1),
  list(name="float stereo", bits=32, channels=2, float=TRUE),
  list(name="16-bit, three channels", bits=16, channels=3),
  list(name="24-bit, three channels", bits=24, channels=3)
)

.longFormatWav <- function(dir, f, ...) {
  return(.longWav(file.path(dir, paste0(gsub("[^a-z0-9]", "", f$name), ".wav")),
                  bits=f$bits, channels=f$channels, float=isTRUE(f$float), ...))
}

# The pipeline id from the outline, as in test-runPipeline.R
.longIds <- function(env=parent.frame()) {
  testthat::local_mocked_bindings(
    .pipelineIdentity=function(p) list(
      id=digest::digest(.outline(p)$text, algo="sha256"), code=c(R="test")
    ),
    .env=env
  )
}

# Count the files read a window at a time, so that a test comparing the two
# ways of reading a file knows the second was taken
.countLong <- function(env=parent.frame()) {
  count <- new.env()
  count$n <- 0
  real <- .longFileWave
  testthat::local_mocked_bindings(.longFileWave=function(...) {
    count$n <- count$n + 1
    real(...)
  }, .env=env)
  return(count)
}

# A wave as a test compares it, without the times its records were made at
.untimed <- function(w) {
  w@processing <- lapply(w@processing, function(r) {
    r$time <- NULL
    r$duration <- NULL
    r
  })
  w@metadata$calculatedMetadata$at <- NULL
  return(w)
}

# The tables of a store, without the times the files were run at
.tables <- function(store, p) {
  res <- pipelineResults(store, p)
  attr(res, "files") <- NULL
  return(res)
}

# Every warning an expression raised, muffled
.warningsOf <- function(expr) {
  found <- list()
  withCallingHandlers(expr, warning=function(cond) {
    found[[length(found) + 1]] <<- cond
    invokeRestart("muffleWarning")
  })
  return(found)
}

# Leave the sizes of a WAV file as a recorder that never finished leaves them:
# a data size of zero, and a RIFF size covering only the header.
.unwritten <- function(path) {
  data <- .wavFindChunk(.wavChunksFile(path)$chunks, "data")
  con <- file(path, "r+b")
  on.exit(close(con))
  seek(con, 4, rw="write")
  writeBin(.uintToRaw(data$start - 9, 4L), con)
  seek(con, data$start - 5, rw="write")
  writeBin(as.raw(c(0, 0, 0, 0)), con)
  return(path)
}

# Cut a WAV file short, holding `held` bytes of its audio
.cutShort <- function(path, held) {
  start <- .wavFindChunk(.wavChunksFile(path)$chunks, "data")$start
  bytes <- readBin(path, "raw", n=file.size(path))
  writeBin(bytes[seq_len(start - 1 + held)], path)
  return(path)
}

test_that("the size of a file once read is worked out from its header", {
  dir <- withr::local_tempdir()
  for (f in .longFormats) {
    path <- .longFormatWav(dir, f, frames=1000)
    w <- readAudio(path)
    samples <- if (methods::is(w, "WaveMC")) w@.Data else w@left
    per <- if (typeof(samples) == "integer") 4 else 8
    size <- .decodedSize(path)
    expect_equal(size$bytes, 1000 * f$channels * per, info=f$name)
    expect_equal(size$perSample, per, info=f$name)
    expect_equal(size$channels, f$channels, info=f$name)
    expect_equal(size$rate, 8000, info=f$name)
    expect_true(is.na(size$reason), info=f$name)
  }
  # A file whose audio size was never written holds the audio to its end.
  unwritten <- .unwritten(.longWav(file.path(dir, "unwritten.wav"), frames=1000))
  expect_equal(.decodedSize(unwritten)$bytes, 4000)
  expect_warning(expect_length(readAudio(unwritten)@left, 1000), "was never written")
  # One that holds no audio, with metadata after it, holds none.
  empty <- mdFile(mdWav(mdFmt(8000), mdChunk("data", raw(0)), mdGuanoChunk("Make:X")))
  expect_equal(.decodedSize(empty)$bytes, 0)
  expect_length(readAudio(empty)@left, 0)
  # A triggered recording is measured as it is once expanded.
  triggered <- system.file("extdata", "AUDIOMOTHT.WAV", package="sonicscrewdriver")
  expanded <- readAudio(triggered)
  expect_equal(.decodedSize(triggered)$bytes, length(expanded@left) * 4)
  expect_gt(length(expanded@left), .wavFramesHeld(triggered)$frames)
  expect_equal(.triggeredGrowth(triggered, chunk=3), .triggeredGrowth(triggered))
})

test_that("only a RIFF WAV file of PCM or floating-point audio can be read in windows", {
  rf64 <- mdFile(mdRf64Wav(1:2000))
  size <- .decodedSize(rf64)
  expect_equal(size$bytes, 2000 * 8)
  expect_equal(size$reason, "a WAV file in the RF64 form is read whole, by the av package")
  triggered <- system.file("extdata", "AUDIOMOTHT.WAV", package="sonicscrewdriver")
  expect_equal(.decodedSize(triggered)$reason,
               "an AudioMoth triggered recording is expanded whole before it is read")

  skip_if_not_installed("av")
  flac <- system.file("extdata", "STEREO.flac", package="sonicscrewdriver")
  size <- .decodedSize(flac)
  expect_equal(size$bytes, length(readAudio(flac)@left) * 2 * 8)
  expect_equal(size$reason, paste(
    "only a WAV file is read a window at a time, as a part of any other format is",
    "not read exactly"
  ))
  mp3 <- system.file("extdata", "STEREO.mp3", package="sonicscrewdriver")
  expect_equal(.decodedSize(mp3)$perSample, 4)
  # A WAV file of audio readWave() does not read, here mu-law, is decoded by av.
  mulaw <- mdFile(mdWav(
    mdChunk("fmt ", c(mdU16(7), mdU16(1), mdU32(8000), mdU32(8000), mdU16(1), mdU16(8))),
    mdChunk("data", as.raw(rep(c(10, 200), 500)))
  ))
  size <- .decodedSize(mulaw)
  expect_equal(size$perSample, 8)
  expect_equal(size$reason,
               "only a WAV file of PCM or floating-point audio is read a window at a time")
  # Nothing is known of a file av cannot read, or of any file but a WAV file
  # without av.
  junk <- mdFile(as.raw(rep(7L, 100)), extension=".ogg")
  expect_null(.decodedSize(junk))
  local_mocked_bindings(package.installed=function(...) FALSE)
  expect_null(.decodedSize(flac))
})

test_that("the digest of a file read a block at a time is that of the whole wave", {
  dir <- withr::local_tempdir()
  cases <- list(
    list(bits=16, channels=1, frames=50000, block=9973),
    list(bits=16, channels=2, frames=50000, block=9973),
    list(bits=24, channels=3, frames=20000, block=4999),
    list(bits=16, channels=1, frames=100, block=7),
    list(bits=16, channels=3, frames=100, block=7)
  )
  for (k in cases) {
    path <- .longWav(tempfile(tmpdir=dir, fileext=".wav"), bits=k$bits, channels=k$channels,
                     frames=k$frames)
    w <- readAudio(path)
    one <- readAudio(path, from=1, to=1, time.unit="samples")
    before <- list.files(tempdir(), pattern="\\.bin$")
    for (method in c("full", "sketch")) {
      expect_identical(.fileWaveHash(path, method, one, k$frames, k$block), .waveHash(w, method),
                       info=paste(k$channels, "channels,", k$frames, "frames,", method))
    }
    # The files the samples were written to are gone, every channel of them.
    expect_setequal(list.files(tempdir(), pattern="\\.bin$"), before)
  }
  # Channels that could not be joined give no digest at all.
  local_mocked_bindings(.appendFile=function(to, from) FALSE)
  expect_error(.fileWaveHash(path, "full", one, 100, 7),
               "Could not join the channels of the recording to digest them.")
})

test_that("windows read from disk are the windows cut from the whole wave", {
  .longIds()
  dir <- withr::local_tempdir()
  seen <- new.env()
  grab <- function(w) {
    seen$windows <- c(seen$windows, list(w))
    length(w)
  }
  p <- pipeline(
    windowing(1000, then=grab()),
    windowing(0.3, time.unit="seconds", window.overlap=0.1, complete.windows=FALSE, then=pipeline(
      grab(), dutyCycle(), windowing(500, then=grab())
    ))
  )
  run <- function(set, store, limit) {
    seen$windows <- NULL
    runPipeline(p, set, store, max.memory=limit, quiet=TRUE)
    return(seen$windows)
  }
  long <- .countLong()
  for (f in .longFormats) {
    path <- .longFormatWav(dir, f)
    set <- recordingSet(path)
    set$lat <- 51.5
    limit <- .decodedSize(path)$bytes / 2
    whole <- run(set, file.path(dir, paste0(f$name, " whole")), Inf)
    disk <- run(set, file.path(dir, paste0(f$name, " disk")), limit)
    expect_length(disk, length(whole))
    expect_equal(lapply(disk, .untimed), lapply(whole, .untimed), info=f$name)
    expect_equal(.tables(file.path(dir, paste0(f$name, " disk")), p),
                 .tables(file.path(dir, paste0(f$name, " whole")), p), info=f$name)
  }
  expect_equal(long$n, length(.longFormats))
  # The record of each cut digests the whole recording as the method in force
  # does.
  withr::local_options(sonicscrewdriver.provenance="full")
  path <- .longFormatWav(dir, .longFormats[[2]])
  whole <- run(recordingSet(path), file.path(dir, "full whole"), Inf)
  disk <- run(recordingSet(path), file.path(dir, "full disk"), .decodedSize(path)$bytes / 2)
  expect_equal(lapply(disk, .untimed), lapply(whole, .untimed))
  expect_equal(disk[[1]]@processing[[1]]$hash_method, "full")
})

test_that("what a whole read says of a file is said once", {
  .longIds()
  dir <- withr::local_tempdir()
  p <- pipeline(windowing(0.1, time.unit="seconds", then=length()))
  long <- .countLong()
  runs <- function(path, name) {
    set <- recordingSet(path)
    limit <- .decodedSize(path)$bytes / 2
    before <- list.files(tempdir(), pattern="\\.wav$")
    whole <- .warningsOf(runPipeline(p, set, file.path(dir, paste(name, "whole")),
                                     max.memory=Inf, quiet=TRUE))
    read <- long$n
    disk <- .warningsOf(runPipeline(p, set, file.path(dir, paste(name, "disk")),
                                    max.memory=limit, quiet=TRUE))
    expect_equal(long$n, read + 1)
    # No copy made to read from is left behind.
    expect_setequal(list.files(tempdir(), pattern="\\.wav$"), before)
    expect_equal(.tables(file.path(dir, paste(name, "disk")), p),
                 .tables(file.path(dir, paste(name, "whole")), p))
    return(list(whole=whole, disk=disk))
  }
  classes <- function(found) vapply(found, function(w) class(w)[1], character(1))

  # A file read from a repaired copy is copied once, however many parts of it
  # are read.
  copies <- new.env()
  copies$n <- 0
  real <- .wavRepaired
  local_mocked_bindings(.wavRepaired=function(...) {
    made <- real(...)
    copies$n <- copies$n + !is.null(made)
    made
  })
  unwritten <- .unwritten(.longWav(file.path(dir, "unwritten.wav")))
  said <- runs(unwritten, "unwritten")
  expect_equal(classes(said$whole), "unwrittenAudioSize")
  expect_equal(classes(said$disk), "unwrittenAudioSize")
  # Said of the file, rather than of the copy read in its place.
  expect_match(conditionMessage(said$disk[[1]]), basename(unwritten), fixed=TRUE)
  # One copy for the whole read, and one for the read a window at a time.
  expect_equal(copies$n, 2)

  # Cut part way through a frame, and so read from a copy holding its whole
  # frames, and cut at the end of one, and read where it is.
  for (held in c(4000 * 4 + 2, 4000 * 4)) {
    cut <- .cutShort(.longWav(file.path(dir, paste0("cut", held, ".wav")), channels=2), held)
    said <- runs(cut, paste("cut", held))
    expect_equal(classes(said$whole), "truncatedAudio")
    expect_equal(classes(said$disk), "truncatedAudio")
  }

  # Samples that are not finite are said of each part read that holds them.
  # tuneR will not write one, so it is written into the bytes.
  nan <- .longWav(file.path(dir, "nan.wav"), bits=32, float=TRUE)
  start <- .wavFindChunk(.wavChunksFile(nan)$chunks, "data")$start
  con <- file(nan, "r+b")
  seek(con, start - 1 + 2999 * 4, rw="write")
  writeBin(NaN, con, size=4, endian="little")
  close(con)
  said <- runs(nan, "nan")
  expect_equal(classes(said$whole), "nonFiniteSamples")
  expect_true(length(said$disk) >= 1)
  expect_true(all(classes(said$disk) == "nonFiniteSamples"))
})

test_that("a long file that cannot be read in windows fails before it is read", {
  .longIds()
  dir <- withr::local_tempdir()
  local_mocked_bindings(readAudio=function(...) stop("The file was read."))
  path <- .longWav(file.path(dir, "a.wav"))
  failure <- function(p, path, limit) {
    store <- tempfile("store", tmpdir=dir)
    runPipeline(p, recordingSet(path), store, max.memory=limit, quiet=TRUE)
    files <- attr(pipelineResults(store, p), "files")
    expect_equal(files$status, "error")
    return(files$error)
  }
  over <- "At 24 kB decoded, the file is over max.memory (10 kB)"
  expect_equal(failure(pipeline(length()), path, 1e4), paste0(
    over, ", and step 1 length() needs all of it at once: only the steps of a ",
    "windowing() given then= are run on such a file, a window at a time."
  ))
  expect_match(
    failure(pipeline(windowing(1000, then=length()), dutyCycle()), path, 1e4),
    ", and step 2 dutyCycle() needs all of it at once", fixed=TRUE
  )
  expect_equal(
    failure(pipeline(windowing(4000, then=length())), path, 1e4),
    "A window of step 1 windowing(4000) takes 16 kB decoded, over max.memory (10 kB)."
  )
  triggered <- system.file("extdata", "AUDIOMOTHT.WAV", package="sonicscrewdriver")
  expect_match(failure(pipeline(windowing(10, then=length())), triggered, 1000),
               ", and an AudioMoth triggered recording is expanded whole before it is read.",
               fixed=TRUE)
  skip_if_not_installed("av")
  for (other in c("STEREO.flac", "STEREO.mp3")) {
    expect_match(
      failure(pipeline(windowing(10, then=length())),
              system.file("extdata", other, package="sonicscrewdriver"), 1000),
      "only a WAV file is read a window at a time", fixed=TRUE
    )
  }
})

test_that("a long file whose run fails part way leaves nothing behind", {
  .longIds()
  dir <- withr::local_tempdir()
  unwritten <- .unwritten(.longWav(file.path(dir, "unwritten.wav")))
  p <- pipeline(windowing(1000, then=length()))
  before <- list.files(tempdir(), pattern="\\.wav$")
  local_mocked_bindings(.fileWaveHash=function(...) stop("The disk is full."))
  suppressWarnings(runPipeline(p, recordingSet(unwritten), file.path(dir, "store"),
                               max.memory=1e4, quiet=TRUE))
  expect_equal(attr(pipelineResults(file.path(dir, "store"), p), "files")$error,
               "The disk is full.")
  expect_setequal(list.files(tempdir(), pattern="\\.wav$"), before)
})

test_that("a windowing the file cannot be windowed by says so as it runs", {
  .longIds()
  dir <- withr::local_tempdir()
  path <- .longWav(file.path(dir, "a.wav"))
  other <- .longWav(file.path(dir, "b.wav"), frames=100, seed=2)
  failed <- function(p) {
    store <- tempfile("store", tmpdir=dir)
    runPipeline(p, recordingSet(c(path, other)), store, max.memory=1e4, quiet=TRUE)
    files <- attr(pipelineResults(store, p), "files")
    # Each file fails as its windowing does, the long one read a window at a
    # time and the other read whole, and the batch goes on past the first.
    expect_equal(files$status, c("error", "error"))
    expect_equal(sub("^.* failed: ", "", files$error[1]), sub("^.* failed: ", "", files$error[2]))
    return(files$error[1])
  }
  expect_match(
    failed(pipeline(windowing(1, time.unit="fortnights", then=length()))),
    "^Step 1 windowing\\(1, time.unit = \"fortnights\"\\) failed: time.unit must be one of"
  )
  for (len in list(NA, c(100, 200))) {
    expect_match(failed(pipeline(windowing(len, then=length()))), "^Step 1 windowing\\(.* failed: ")
  }
})

test_that("a file whose size cannot be told is read whole", {
  .longIds()
  dir <- withr::local_tempdir()
  path <- .longWav(file.path(dir, "a.wav"))
  p <- pipeline(length())
  for (unknown in list(function(file) NULL, function(file) stop("No header."))) {
    local_mocked_bindings(.decodedSize=unknown)
    store <- tempfile("store", tmpdir=dir)
    runPipeline(p, recordingSet(path), store, max.memory=1, quiet=TRUE)
    expect_equal(pipelineResults(store, p)[[1]]$value, 6000)
  }
})

test_that("max.memory is a number of bytes, or Inf", {
  set <- recordingSet(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  for (bad in list(0, -1, NA, "4e9", c(1, 2))) {
    expect_error(runPipeline(pipeline(length()), set, tempfile(), max.memory=bad),
                 "max.memory must be a number of bytes greater than 0, or Inf.", fixed=TRUE)
  }
})
