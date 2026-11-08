# A short wave file of noise, different for each seed
.writeTestWav <- function(path, seconds=2, seed=1, rate=8000) {
  set.seed(seed)
  w <- tuneR::noise("white", duration=rate * seconds, samp.rate=rate, bit=16, pcm=TRUE)
  tuneR::writeWave(tuneR::normalize(w, "16"), path)
  return(path)
}

.sha256 <- function(path) digest::digest(file=path, algo="sha256")

# Working out an id hashes the code of every package a step uses, which takes
# seconds, so most tests stand in an id made from the outline. One test below
# runs with the real id.
.quickIds <- function(env=parent.frame()) {
  testthat::local_mocked_bindings(
    .pipelineIdentity=function(p) list(
      id=digest::digest(.outline(p)$text, algo="sha256"), code=c(R="test")
    ),
    .env=env
  )
}

.windowsTable <- 'windowing(1, time.unit = "seconds") / dutyCycle()'

test_that("runPipeline() runs every file and pipelineResults() reads the tables back", {
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"), seconds=2, seed=1)
  fb <- .writeTestWav(file.path(dir, "b.wav"), seconds=3, seed=2)
  set <- recordingSet(c(fa, fb), source="test")
  set$start <- as.POSIXct(c("2026-05-01 04:00:00", NA), tz="UTC")
  p <- pipeline(length(), windowing(1, time.unit="seconds", then=dutyCycle()))
  store <- file.path(dir, "store")

  msgs <- capture_messages(out <- runPipeline(p, set, store))
  id <- .pipelineId(p)
  expect_equal(msgs, c(
    paste0("Pipeline ", substr(id, 1, 8), ": 2 files to run, 0 already in the store.\n"),
    "Batch 1 of 1: 2 files, 0 failed.\n"
  ))
  # The set comes back with the hashes of its local files.
  expect_s3_class(out, "RecordingSet")
  expect_equal(out$hash, c(.sha256(fa), .sha256(fb)))
  expect_true(file.exists(file.path(store, id, "pipeline.rds")))

  res <- pipelineResults(store, p)
  expect_named(res, c("length()", .windowsTable))
  expect_named(res[["length()"]], c("file", "pipeline", "error", "value"))
  expect_equal(res[["length()"]]$file, out$hash)
  expect_equal(res[["length()"]]$pipeline, rep(id, 2))
  expect_equal(res[["length()"]]$error, rep(NA_character_, 2))
  expect_equal(res[["length()"]]$value, c(16000, 24000))

  windows <- res[[.windowsTable]]
  expect_named(windows, c(
    "file", "pipeline", "window_start", "window_end", "window_length", "window_time",
    "error", "value"
  ))
  expect_equal(windows$file, rep(out$hash, c(2, 3)))
  expect_equal(windows$window_start, c(0, 1, 0, 1, 2))
  expected <- unlist(c(
    windowing(readAudio(fa), 1, FUN=function(w, ...) dutyCycle(w), time.unit="seconds"),
    windowing(readAudio(fb), 1, FUN=function(w, ...) dutyCycle(w), time.unit="seconds")
  ))
  expect_equal(windows$value, expected)
  # Clock times only where the set gives the recording's start.
  expect_equal(
    windows$window_time,
    as.POSIXct(c("2026-05-01 04:00:00", "2026-05-01 04:00:01", NA, NA, NA), tz="UTC")
  )

  files <- attr(res, "files")
  expect_named(files, c("file", "uri", "location", "status", "error", "time", "batch"))
  expect_equal(files$file, out$hash)
  expect_equal(files$location, set$location)
  expect_equal(files$status, c("ok", "ok"))

  # Results join back to the recordings by hash.
  joined <- merge(res[["length()"]], out, by.x="file", by.y="hash")
  expect_equal(joined$uri[order(joined$value)], set$uri)
})

test_that("a second run skips what the store holds, and retry runs failed files again", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"), seconds=2, seed=1)
  fb <- .writeTestWav(file.path(dir, "b.wav"), seconds=3, seed=2)
  set <- recordingSet(c(fa, fb))
  flag <- file.path(dir, "flag")
  file.create(flag)
  p <- pipeline(length(), (function(w) {
    if (length(w) > 20000 && file.exists(flag)) stop("the flag is up")
    1
  })())
  store <- file.path(dir, "store")

  msgs <- capture_messages(runPipeline(p, set, store))
  expect_equal(msgs[2], "Batch 1 of 1: 2 files, 1 failed.\n")
  res <- pipelineResults(store, p)
  files <- attr(res, "files")
  expect_equal(files$status, c("ok", "error"))
  expect_match(files$error[2], "^Step 2 .* failed: the flag is up$")
  # A failed file has a row in every table, with only its error.
  for (table in res) {
    expect_equal(table$file, c(.sha256(fa), .sha256(fb)))
    expect_equal(table$error, c(NA, files$error[2]))
    expect_equal(table$value[2], NA_real_)
  }

  msgs <- capture_messages(runPipeline(p, set, store))
  expect_match(msgs, "0 files to run, 2 already in the store")

  unlink(flag)
  msgs <- capture_messages(runPipeline(p, set, store, retry=TRUE))
  expect_match(msgs[1], "1 file to run, 1 already in the store")
  expect_equal(msgs[2], "Batch 1 of 1: 1 file, 0 failed.\n")
  res <- pipelineResults(store, p)
  expect_equal(attr(res, "files")$status, c("ok", "ok"))
  expect_equal(attr(res, "files")$batch, c(1, 2))
  expect_equal(res[["length()"]]$value, c(16000, 24000))
  expect_equal(res[["length()"]]$error, c(NA_character_, NA_character_))
  expect_equal(res[[2]]$value, c(1, 1))
})

test_that("batches are written one at a time, and a stopped batch is run again", {
  .quickIds()
  dir <- withr::local_tempdir()
  paths <- vapply(1:3, function(i) .writeTestWav(file.path(dir, paste0(i, ".wav")), seed=i), "")
  set <- recordingSet(paths)
  p <- pipeline(length())
  store <- file.path(dir, "store")
  msgs <- capture_messages(runPipeline(p, set, store, batch=2))
  expect_equal(msgs[-1], c("Batch 1 of 2: 2 files, 0 failed.\n", "Batch 2 of 2: 1 file, 0 failed.\n"))
  batches <- file.path(store, .pipelineIdentity(p)$id, "batches")
  expect_setequal(list.files(batches), c("000001.rds", "000001_1.rds", "000002.rds", "000002_1.rds"))

  # A batch whose list of files was never written is taken not to have run.
  unlink(file.path(batches, "000002.rds"))
  msgs <- capture_messages(runPipeline(p, set, store, batch=2))
  expect_match(msgs[1], "1 file to run, 2 already in the store")
  expect_equal(nrow(pipelineResults(store, p)[[1]]), 3)
  expect_true(file.exists(file.path(batches, "000002.rds")))
})

test_that("copies of one file are run once, with the columns all of them agree on", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"))
  copy <- file.path(dir, "copy.wav")
  file.copy(fa, copy)
  set <- recordingSet(c(fa, copy))
  # The copies disagree on when the recording began.
  set$start <- as.POSIXct(c("2026-05-01 04:00:00", "2026-05-01 05:00:00"), tz="UTC")
  p <- pipeline(windowing(1, time.unit="seconds", then=length()))
  for (size in c(1000, 1)) {
    store <- file.path(dir, paste0("store", size))
    msgs <- capture_messages(out <- runPipeline(p, set, store, batch=size))
    expect_match(msgs[1], "1 file to run, 0 already in the store")
    expect_equal(out$hash, rep(.sha256(fa), 2))
    res <- pipelineResults(store, p)
    expect_equal(res[[1]]$file, rep(.sha256(fa), 2))
    expect_false("window_time" %in% names(res[[1]]))
    expect_equal(nrow(attr(res, "files")), 1)
  }
})

test_that("of two downloads that hold the same bytes, the rows of one that ran are kept", {
  .quickIds()
  dir <- withr::local_tempdir()
  src <- .writeTestWav(file.path(dir, "src.wav"))
  local_mocked_bindings(.audioblast_download=function(url, path) {
    file.copy(src, path)
    invisible(path)
  })
  urls <- c("https://example.org/a/one.wav", "https://example.org/b/two.wav")
  set <- recordingSet(data.frame(uri=c("r1", "r2"), location=urls))
  for (failing in c("/a/", "/b/")) {
    p <- pipeline((function(w) {
      if (grepl(failing, w@origin, fixed=TRUE)) stop("not this one")
      length(w)
    })())
    for (size in c(1000, 1)) {
      store <- tempfile("store", tmpdir=dir)
      out <- runPipeline(p, set, store, batch=size, quiet=TRUE)
      expect_equal(out$hash, rep(.sha256(src), 2))
      res <- pipelineResults(store, p)
      files <- attr(res, "files")
      # One file, which ran, with the rows of the download that did not fail.
      expect_equal(files$status, "ok")
      expect_equal(files$location, urls[!grepl(failing, urls, fixed=TRUE)])
      expect_equal(res[[1]]$file, .sha256(src))
      expect_equal(res[[1]]$value, 16000)
      expect_equal(res[[1]]$error, NA_character_)
    }
  }
  # Where both run, the rows of the first are kept, once.
  p <- pipeline(length())
  runPipeline(p, set, file.path(dir, "both"), quiet=TRUE)
  res <- pipelineResults(file.path(dir, "both"), p)
  expect_equal(res[[1]]$value, 16000)
})

test_that("a download that failed and then ran is taken for one file", {
  .quickIds()
  dir <- withr::local_tempdir()
  src <- .writeTestWav(file.path(dir, "src.wav"))
  up <- FALSE
  local_mocked_bindings(.audioblast_download=function(url, path) {
    if (!up) stop("HTTP 503")
    file.copy(src, path)
    invisible(path)
  })
  url <- "https://example.org/files/later.wav"
  set <- recordingSet(data.frame(uri="r", location=url))
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  expect_equal(attr(pipelineResults(store, p), "files")$status, "error")
  up <- TRUE
  out <- runPipeline(p, set, store, retry=TRUE, quiet=TRUE)
  expect_equal(out$hash, .sha256(src))
  res <- pipelineResults(store, p)
  files <- attr(res, "files")
  expect_equal(nrow(files), 1)
  expect_equal(files$status, "ok")
  expect_equal(files$batch, 2)
  # The failure before its hash was known leaves no error row behind.
  expect_equal(res[[1]]$file, .sha256(src))
  expect_equal(res[[1]]$error, NA_character_)
})

test_that("downloads of the same name from different places are kept apart", {
  .quickIds()
  dir <- withr::local_tempdir()
  srcs <- c(.writeTestWav(file.path(dir, "s1.wav"), seconds=2, seed=1),
            .writeTestWav(file.path(dir, "s2.wav"), seconds=3, seed=2))
  urls <- c("https://example.org/site-a/audio.wav", "https://example.org/site-b/audio.wav")
  local_mocked_bindings(.audioblast_download=function(url, path) {
    file.copy(srcs[match(url, urls)], path)
    invisible(path)
  })
  set <- recordingSet(data.frame(uri=c("a", "b"), location=urls))
  keep <- file.path(dir, "keep")
  p <- pipeline(length())
  out <- runPipeline(p, set, file.path(dir, "store"), keep=keep, quiet=TRUE)
  expect_equal(out$hash, c(.sha256(srcs[1]), .sha256(srcs[2])))
  expect_equal(pipelineResults(file.path(dir, "store"), p)[[1]]$value, c(16000, 24000))
  kept <- list.files(keep, recursive=TRUE)
  expect_length(kept, 2)
  expect_equal(basename(kept), c("audio.wav", "audio.wav"))
})

test_that("a file moved since it was run is found in the store, and comes back with its hash", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"))
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, recordingSet(fa), store, quiet=TRUE)
  moved <- file.path(dir, "moved.wav")
  file.rename(fa, moved)
  msgs <- capture_messages(out <- runPipeline(p, recordingSet(moved), store))
  expect_match(msgs[1], "0 files to run, 1 already in the store")
  expect_equal(out$hash, .sha256(moved))
})

test_that("files without a location are not taken for one another", {
  .quickIds()
  dir <- withr::local_tempdir()
  given <- strrep("a", 64)
  set <- recordingSet(data.frame(uri=c("hashed", "unknown"), location=NA_character_,
                                 hash=c(given, NA)))
  p <- pipeline(length())
  store <- file.path(dir, "store")
  out <- runPipeline(p, set, store, quiet=TRUE)
  expect_equal(out$hash, c(given, NA))
  files <- attr(pipelineResults(store, p), "files")
  expect_equal(files$uri, c("hashed", "unknown"))
  expect_equal(files$file, c(given, NA))
  msgs <- capture_messages(runPipeline(p, set, store))
  expect_match(msgs, "0 files to run, 2 already in the store")
})

test_that("a set cut down to its uri and location is run", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"))
  cut <- recordingSet(fa)[, c("uri", "location")]
  expect_s3_class(cut, "RecordingSet")
  expect_false("hash" %in% names(cut))
  p <- pipeline(length())
  out <- runPipeline(p, cut, file.path(dir, "store"), quiet=TRUE)
  expect_equal(out$hash, .sha256(fa))
  expect_equal(pipelineResults(file.path(dir, "store"), p)[[1]]$value, 16000)
})

test_that("a local file that failed before it could be hashed waits for retry", {
  .quickIds()
  dir <- withr::local_tempdir()
  later <- file.path(dir, "later.wav")
  set <- recordingSet(data.frame(uri="later", location=later))
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  expect_equal(attr(pipelineResults(store, p), "files")$file, NA_character_)
  # Now there, and hashed before the run, it is still the file that failed.
  .writeTestWav(later)
  msgs <- capture_messages(runPipeline(p, set, store))
  expect_match(msgs, "0 files to run, 1 already in the store")
  msgs <- capture_messages(runPipeline(p, set, store, retry=TRUE))
  expect_match(msgs[1], "1 file to run, 0 already in the store")
  files <- attr(pipelineResults(store, p), "files")
  expect_equal(files$file, .sha256(later))
  expect_equal(files$status, "ok")
})

test_that("a relative file hashed only when it is run comes back with its hash", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "rel.wav"))
  withr::local_dir(dir)
  real <- .audioblast_sha256
  calls <- 0
  local_mocked_bindings(.audioblast_sha256=function(path) {
    calls <<- calls + 1
    if (calls == 1) stop("busy")
    real(path)
  })
  set <- recordingSet(data.frame(uri="r", location="rel.wav"))
  out <- runPipeline(pipeline(length()), set, "store", quiet=TRUE)
  # Not hashed before the run, and hashed when it was read.
  expect_equal(calls, 2)
  expect_equal(out$hash, .sha256(fa))
  expect_equal(out$location, "rel.wav")
})

test_that("a file that is not there yet has the path it will have once it is", {
  dir <- withr::local_tempdir()
  long <- normalizePath(dir, winslash="/")
  withr::local_dir(dir)
  expect_equal(.absolutePath(c("later.wav", "sub/deeper/later.wav")),
               paste0(long, c("/later.wav", "/sub/deeper/later.wav")))
  # A folder given by its short name, as Windows can give a temporary folder,
  # is given in full, as it is once the file is there.
  short <- if (.Platform$OS.type == "windows") utils::shortPathName(dir) else dir
  expect_equal(.absolutePath(file.path(short, "later.wav")), paste0(long, "/later.wav"))
  writeLines("", "later.wav")
  expect_equal(.absolutePath(file.path(short, "later.wav")), paste0(long, "/later.wav"))
  # Under the root of a drive, with one "/" after it.
  root <- long
  while (dirname(root) != root) root <- dirname(root)
  missing <- paste0(sub("/+$", "", root), "/", basename(tempfile("absent")), "/x.wav")
  expect_equal(.absolutePath(missing), missing)
})

test_that("a column whose name begins with start gives no clock time", {
  .quickIds()
  dir <- withr::local_tempdir()
  set <- recordingSet(.writeTestWav(file.path(dir, "a.wav")))
  set$start_offset <- 5
  p <- pipeline(windowing(1, time.unit="seconds", then=dutyCycle()))
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  expect_false("window_time" %in% names(pipelineResults(store, p)[[1]]))
})

test_that("only a hash read from a file is taken for the hash at its location", {
  .quickIds()
  dir <- withr::local_tempdir()
  src <- .writeTestWav(file.path(dir, "src.wav"))
  local_mocked_bindings(.audioblast_download=function(url, path) {
    file.copy(src, path)
    invisible(path)
  })
  url <- "https://example.org/files/a.wav"
  p <- pipeline(length())
  store <- file.path(dir, "store")
  # A wrong hash fails the file, and is not then taken to be what is there.
  runPipeline(p, recordingSet(data.frame(uri="u", location=url, hash=strrep("0", 64))),
              store, quiet=TRUE)
  unhashed <- recordingSet(data.frame(uri="u", location=url))
  out <- runPipeline(p, unhashed, store, quiet=TRUE)
  expect_equal(out$hash, .sha256(src))
  files <- attr(pipelineResults(store, p), "files")
  expect_equal(files$status[files$file == .sha256(src)], "ok")

  # A file that was read before it failed is known by the hash read from it.
  failing <- pipeline((function(w) stop("no"))())
  store2 <- file.path(dir, "store2")
  out <- runPipeline(failing, unhashed, store2, quiet=TRUE)
  expect_equal(out$hash, .sha256(src))
  msgs <- capture_messages(runPipeline(failing, unhashed, store2))
  expect_match(msgs, "0 files to run, 1 already in the store")
})

test_that("pipelines that share an id share the names of their tables", {
  dir <- withr::local_tempdir()
  # Calls written differently, as these are, give the same id.
  testthat::local_mocked_bindings(
    .pipelineIdentity=function(p) list(id=strrep("c", 64), code=c(R="test"))
  )
  fa <- .writeTestWav(file.path(dir, "a.wav"), seed=1)
  fb <- .writeTestWav(file.path(dir, "b.wav"), seed=2)
  positional <- pipeline(dutyCycle(0.2))
  named <- pipeline(dutyCycle(limit=0.2))
  store <- file.path(dir, "store")
  runPipeline(positional, recordingSet(fa), store, quiet=TRUE)
  runPipeline(named, recordingSet(fb), store, quiet=TRUE)
  for (p in list(positional, named)) {
    res <- pipelineResults(store, p)
    expect_named(res, "dutyCycle(0.2)")
    expect_equal(res[[1]]$file, c(.sha256(fa), .sha256(fb)))
    expect_equal(pipelineResults(store, p, step="1"), res[[1]])
  }
})

test_that("a clock time found only in a later file stays among the keys", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"), seconds=2, seed=1)
  fb <- .writeTestWav(file.path(dir, "b.wav"), seconds=2, seed=2)
  set <- recordingSet(c(fa, fb))
  set$start <- as.POSIXct(c(NA, "2026-05-01 04:00:00"), tz="UTC")
  p <- pipeline(windowing(1, time.unit="seconds", then=dutyCycle()))
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  windows <- pipelineResults(store, p)[[1]]
  expect_named(windows, c(
    "file", "pipeline", "window_start", "window_end", "window_length", "window_time",
    "error", "value"
  ))
  expect_equal(
    windows$window_time,
    as.POSIXct(c(NA, NA, "2026-05-01 04:00:00", "2026-05-01 04:00:01"), tz="UTC")
  )
})

test_that("a file that cannot be hashed before the run fails alone", {
  .quickIds()
  dir <- withr::local_tempdir()
  good <- .writeTestWav(file.path(dir, "good.wav"))
  locked <- .writeTestWav(file.path(dir, "locked.wav"), seed=2)
  real <- .audioblast_sha256
  local_mocked_bindings(.audioblast_sha256=function(path) {
    if (grepl("locked", path, fixed=TRUE)) stop("Permission denied")
    real(path)
  })
  p <- pipeline(length())
  store <- file.path(dir, "store")
  out <- runPipeline(p, recordingSet(c(good, locked)), store, quiet=TRUE)
  files <- attr(pipelineResults(store, p), "files")
  expect_equal(files$status, c("ok", "error"))
  expect_equal(files$error[2], "Permission denied")
  expect_equal(out$hash, c(.sha256(good), NA))
})

test_that("local locations reach the batches as absolute paths", {
  .quickIds()
  dir <- withr::local_tempdir()
  .writeTestWav(file.path(dir, "rel.wav"))
  withr::local_dir(dir)
  set <- recordingSet(data.frame(uri="r", location="rel.wav"))
  given <- NULL
  local_mocked_bindings(.runPipelineBatch=function(job, p, id, dir, keep, max.memory) {
    given <<- job
    data.frame(status="ok")
  })
  out <- runPipeline(pipeline(length()), set, "store", quiet=TRUE)
  expect_equal(given$files$location, normalizePath(file.path(dir, "rel.wav"), winslash="/"))
  expect_equal(given$rows$location, given$files$location)
  # The set comes back with its locations as they were given.
  expect_equal(out$location, "rel.wav")
})

test_that("store files are written whole or not at all", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "x.rds")
  .saveWhole(list(a=1), path)
  expect_equal(readRDS(path), list(a=1))
  expect_equal(list.files(dir), "x.rds")
  # A file that cannot be put in place leaves nothing behind.
  blocked <- file.path(dir, "blocked.rds")
  dir.create(blocked)
  expect_error(.saveWhole(1, blocked), paste0("Could not write ", blocked, "."), fixed=TRUE)
  expect_setequal(list.files(dir), c("x.rds", "blocked.rds"))
})

test_that("a file left half written in a store is not read", {
  .quickIds()
  dir <- withr::local_tempdir()
  set <- recordingSet(.writeTestWav(file.path(dir, "a.wav")))
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  batches <- file.path(store, .pipelineIdentity(p)$id, "batches")
  writeLines("half", file.path(batches, "000002.rds.1a2b.part"))
  expect_equal(nrow(pipelineResults(store, p)[[1]]), 1)
  msgs <- capture_messages(runPipeline(p, set, store))
  expect_match(msgs, "0 files to run, 1 already in the store")
})

test_that("files at a URL are downloaded, checked and deleted unless kept", {
  .quickIds()
  dir <- withr::local_tempdir()
  src <- .writeTestWav(file.path(dir, "src.wav"))
  fetched <- character(0)
  local_mocked_bindings(.audioblast_download=function(url, path) {
    fetched <<- c(fetched, path)
    file.copy(src, path)
    invisible(path)
  })
  url <- "https://example.org/files/a.wav"
  set <- recordingSet(data.frame(
    uri="https://example.org/recording/1", location=url, hash=toupper(.sha256(src))
  ))
  p <- pipeline(length())

  runPipeline(p, set, file.path(dir, "store1"), quiet=TRUE)
  expect_length(fetched, 1)
  expect_equal(basename(fetched), "a.wav")
  expect_false(dir.exists(dirname(fetched)))
  res <- pipelineResults(file.path(dir, "store1"), p)
  expect_equal(res[[1]]$file, .sha256(src))
  expect_equal(attr(res, "files")$location, url)

  # Kept, and not fetched again where it is already there.
  keep <- file.path(dir, "keep")
  runPipeline(p, set, file.path(dir, "store2"), keep=keep, quiet=TRUE)
  expect_equal(basename(list.files(keep, recursive=TRUE)), "a.wav")
  runPipeline(p, set, file.path(dir, "store3"), keep=keep, quiet=TRUE)
  expect_length(fetched, 2)
  expect_equal(pipelineResults(file.path(dir, "store3"), p)[[1]]$value, 16000)

  # Without a hash in the set, the file is known by its location once run.
  unhashed <- recordingSet(data.frame(uri="https://example.org/recording/1", location=url))
  out <- runPipeline(p, unhashed, file.path(dir, "store4"), quiet=TRUE)
  expect_equal(out$hash, .sha256(src))
  msgs <- capture_messages(runPipeline(p, unhashed, file.path(dir, "store4")))
  expect_match(msgs, "0 files to run, 1 already in the store")

  # A download that does not match its hash fails.
  wrong <- recordingSet(data.frame(uri="u", location=url, hash=strrep("0", 64)))
  runPipeline(p, wrong, file.path(dir, "store5"), quiet=TRUE)
  files <- attr(pipelineResults(file.path(dir, "store5"), p), "files")
  expect_equal(files$file, strrep("0", 64))
  expect_equal(files$error, paste0(
    "The file at ", url, " has the hash ", .sha256(src), ", not the ",
    strrep("0", 64), " the set gives it."
  ))
})

test_that("a download that fails is recorded and the run moves on", {
  .quickIds()
  dir <- withr::local_tempdir()
  local_mocked_bindings(.audioblast_download=function(url, path) stop("HTTP 404"))
  url <- "https://example.org/files/gone.wav"
  set <- rbind(
    recordingSet(data.frame(uri="https://example.org/recording/2", location=url)),
    recordingSet(.writeTestWav(file.path(dir, "a.wav")))
  )
  p <- pipeline(length())
  for (keep in list(NULL, file.path(dir, "keep"))) {
    store <- tempfile("store", tmpdir=dir)
    runPipeline(p, set, store, keep=keep, quiet=TRUE)
    res <- pipelineResults(store, p)
    files <- attr(res, "files")
    expect_equal(files$status, c("error", "ok"))
    expect_equal(files$error[1], paste0("Could not download ", url, ": HTTP 404"))
    expect_equal(res[[1]]$file, c(.sha256(file.path(dir, "a.wav")), NA))
    expect_equal(res[[1]]$error, c(NA, files$error[1]))
  }
  expect_length(list.files(file.path(dir, "keep"), recursive=TRUE), 0)
})

test_that("files that cannot be found, read or matched to their hash fail", {
  .quickIds()
  dir <- withr::local_tempdir()
  good <- .writeTestWav(file.path(dir, "good.wav"))
  bad <- file.path(dir, "bad.wav")
  writeLines("This is not audio.", bad)
  gone <- .writeTestWav(file.path(dir, "gone.wav"), seed=3)
  elsewhere <- file.path(dir, "elsewhere.wav")
  set <- rbind(
    recordingSet(c(good, bad, gone)),
    recordingSet(data.frame(uri=c("x1", "x2"), location=elsewhere,
                            hash=c(strrep("1", 64), strrep("2", 64)))),
    recordingSet(data.frame(uri="nowhere", location=NA_character_))
  )
  unlink(gone)
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  res <- pipelineResults(store, p)
  files <- attr(res, "files")
  rownames(files) <- files$uri
  expect_equal(files$status, c("ok", rep("error", 4)))
  expect_equal(res[[1]]$file, c(.sha256(good), .sha256(bad), NA, NA, NA))
  expect_false(is.na(files[set$uri[2], "error"]))
  expect_equal(files[set$uri[3], "error"], paste0("There is no file at ", set$location[3], "."))
  # Two recordings of one file that give it two hashes.
  expect_equal(files["x1", "error"], "The set gives this file 2 different hashes.")
  expect_equal(files["nowhere", "error"], "The set gives no location for this file.")
  # None is run again: each is found by its hash, its location or its uri.
  msgs <- capture_messages(runPipeline(p, set, store))
  expect_match(msgs, "0 files to run, 5 already in the store")
})

test_that("a wrong hash for a local file fails it", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"))
  set <- recordingSet(data.frame(uri="a", location=fa, hash=strrep("f", 64)))
  runPipeline(pipeline(length()), set, file.path(dir, "store"), quiet=TRUE)
  files <- attr(pipelineResults(file.path(dir, "store"), pipeline(length())), "files")
  expect_match(files$error, paste0("has the hash ", .sha256(fa), ", not the "))
})

test_that("each file is tagged with its location and what its recordings agree on", {
  .quickIds()
  dir <- withr::local_tempdir()
  fa <- .writeTestWav(file.path(dir, "a.wav"))
  set <- recordingSet(data.frame(
    uri=c("r1", "r2"), location=fa, lat=51.5, taxon=c("Gryllus", "Acheta"),
    start=as.POSIXct(c("2026-05-01 04:00:00", "2026-05-01 05:00:00"), tz="UTC")
  ))
  p <- pipeline(
    (function(w) list(
      origin=w@origin, lat=w@metadata$recordingSet$lat,
      has_taxon=!is.null(w@metadata$recordingSet$taxon),
      hash=w@metadata$calculatedMetadata$hash
    ))(),
    windowing(1, time.unit="seconds", then=length())
  )
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  res <- pipelineResults(store, p)
  # Its location, made absolute.
  expect_equal(res[[1]]$value.origin, normalizePath(fa, winslash="/"))
  expect_equal(res[[1]]$value.lat, 51.5)
  expect_false(res[[1]]$value.has_taxon)
  expect_equal(res[[1]]$value.hash, .sha256(fa))
  # The recordings disagree on when the file began, so no clock time is given.
  expect_false("window_time" %in% names(res[[2]]))
})

test_that("pipelineResults() reads one table, by name or number", {
  .quickIds()
  dir <- withr::local_tempdir()
  set <- recordingSet(.writeTestWav(file.path(dir, "a.wav")))
  p <- pipeline(length(), windowing(1, time.unit="seconds", then=dutyCycle()))
  store <- file.path(dir, "store")
  runPipeline(p, set, store, quiet=TRUE)
  all <- pipelineResults(store, p)
  expect_equal(pipelineResults(store, p, step="2.1"), all[[.windowsTable]])
  expect_equal(pipelineResults(store, p, step="length()"), all[["length()"]])
  expect_error(
    pipelineResults(store, p, step="3"),
    paste0("The store holds no table called 3. It holds: length(), ", .windowsTable, "."),
    fixed=TRUE
  )
  expect_error(pipelineResults(store, p, step=1),
               "step must be the name of one table, or its number in the outline.")

  # By id, which is the name of the pipeline's folder in the store.
  id <- .pipelineIdentity(p)$id
  expect_equal(pipelineResults(store, id), all)
  expect_error(pipelineResults(store, list()), "p must be a Pipeline, or the id of one.")
  expect_error(pipelineResults(store, pipeline(dutyCycle())), paste0(
    "It holds the results of ", id, ". An id changes"
  ))
  expect_error(pipelineResults(file.path(dir, "empty"), id), "It holds none at all.")
  expect_error(pipelineResults(NA, id), "store must be the path of a folder.")
})

test_that("a store in which every file failed holds no tables", {
  .quickIds()
  dir <- withr::local_tempdir()
  bad <- file.path(dir, "bad.wav")
  writeLines("This is not audio.", bad)
  p <- pipeline(length())
  store <- file.path(dir, "store")
  runPipeline(p, recordingSet(bad), store, quiet=TRUE)
  res <- pipelineResults(store, p)
  expect_length(res, 0)
  expect_equal(attr(res, "files")$status, "error")
  expect_error(pipelineResults(store, p, step="1"), "It holds: none.")

  # A batch in which every file failed has no table, and the table comes from
  # the other batches.
  good <- .writeTestWav(file.path(dir, "good.wav"))
  runPipeline(p, recordingSet(c(bad, good)), store, quiet=TRUE)
  res <- pipelineResults(store, p)
  expect_equal(attr(res, "files")$batch, c(1, 2))
  expect_equal(res[[1]]$file, c(.sha256(good), .sha256(bad)))
  expect_equal(res[[1]]$error, c(NA, attr(res, "files")$error[1]))
})

test_that("batches can be run on a cluster", {
  .quickIds()
  dir <- withr::local_tempdir()
  paths <- vapply(1:3, function(i) .writeTestWav(file.path(dir, paste0(i, ".wav")), seed=i), "")
  set <- recordingSet(paths)
  p <- pipeline(length())
  testthat::local_mocked_bindings(
    parLapply=function(cl, X, fun, ...) lapply(X, fun, ...),
    .package="parallel"
  )
  msgs <- capture_messages(runPipeline(p, set, file.path(dir, "store"), batch=2, cl="workers"))
  expect_equal(msgs[2], "2 batches: 3 files, 0 failed.\n")
  expect_equal(nrow(pipelineResults(file.path(dir, "store"), p)[[1]]), 3)
  expect_silent(runPipeline(p, set, file.path(dir, "store2"), batch=3, cl="workers", quiet=TRUE))
  msgs <- capture_messages(runPipeline(p, set[1, ], file.path(dir, "store3"), cl="workers"))
  expect_equal(msgs[2], "1 batch: 1 file, 0 failed.\n")
  # Nothing to run sends nothing to the cluster.
  expect_length(capture_messages(runPipeline(p, set, file.path(dir, "store"), cl="workers")), 1)
})

test_that("runPipeline() checks what it is given", {
  set <- recordingSet(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  p <- pipeline(length())
  store <- tempfile()
  expect_error(runPipeline(list(), set, store), "runPipeline() runs a Pipeline, not a list.", fixed=TRUE)
  expect_error(runPipeline(p, data.frame(), store),
               "runPipeline() runs over a RecordingSet, not a data.frame. recordingSet() makes one.",
               fixed=TRUE)
  expect_error(runPipeline(p, set, c("a", "b")), "store must be the path of a folder.")
  expect_error(runPipeline(p, set, ""), "store must be the path of a folder.")
  for (bad in list(0, 1.5, "10", c(1, 2), NA)) {
    expect_error(runPipeline(p, set, store, batch=bad),
                 "batch must be a whole number of files, at least 1.")
  }
  expect_error(runPipeline(p, set, store, keep=1), "keep must be the path of a folder.")
  expect_error(runPipeline(p, set, store, retry="yes"), "retry must be TRUE or FALSE.")
  expect_false(dir.exists(store))
  # A folder inside a file cannot be made.
  blocker <- withr::local_tempfile()
  writeLines("", blocker)
  inside <- file.path(blocker, "store")
  expect_error(runPipeline(p, set, inside), paste0("Could not make the folder ", inside, "."),
               fixed=TRUE)
  expect_error(runPipeline(p, set, store, keep=inside),
               paste0("Could not make the folder ", inside, "."), fixed=TRUE)
})

test_that("the store and the folder for downloads are given to the batches as absolute paths", {
  .quickIds()
  dir <- withr::local_tempdir()
  set <- recordingSet(.writeTestWav(file.path(dir, "a.wav")))
  given <- list()
  local_mocked_bindings(.runPipelineBatch=function(job, p, id, dir, keep, max.memory) {
    given <<- list(dir=dir, keep=keep)
    data.frame(status="ok")
  })
  withr::local_dir(dir)
  runPipeline(pipeline(length()), set, "store", keep="keep", quiet=TRUE)
  expect_equal(dirname(given$dir), normalizePath(file.path(dir, "store"), winslash="/"))
  expect_equal(given$keep, normalizePath(file.path(dir, "keep"), winslash="/"))
})
