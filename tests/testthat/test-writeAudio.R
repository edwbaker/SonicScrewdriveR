test_that("writeAudio writes WAVE files", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))

  f <- tempfile(fileext=".wav")
  expect_equal(writeAudio(w, f), f)
  expect_true(file.exists(f))

  r <- readAudio(f)
  expect_equal(r@left, w@left)
  expect_equal(r@samp.rate, w@samp.rate)
  expect_equal(r@bit, w@bit)
  expect_equal(r@stereo, w@stereo)
  unlink(f)
})

test_that("writeAudio writes stereo and multichannel waves", {
  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"))
  f <- tempfile(fileext=".wav")
  writeAudio(w, f)
  r <- readAudio(f)
  expect_equal(r@left, w@left)
  expect_equal(r@right, w@right)
  unlink(f)

  d <- matrix(rep(1:100, 3), ncol=3)
  colnames(d) <- c("FL", "FR", "FC")
  mc <- tuneR::WaveMC(d, samp.rate=44100, bit=16)
  f <- tempfile(fileext=".wav")
  writeAudio(mc, f)
  r <- tuneR::readWave(f, toWaveMC=TRUE)
  expect_equal(ncol(r@.Data), 3)
  expect_equal(r@.Data[,1], d[,1])
  unlink(f)
})

test_that("writeAudio writes tagged waves as audio", {
  w <- tagWave(tuneR::sine(440, duration=1000))
  f <- tempfile(fileext=".wav")
  writeAudio(w, f)
  expect_true(file.exists(f))
  expect_false(is(readAudio(f), "TaggedWave"))
  unlink(f)
})

test_that("writeAudio uses the mime argument in preference to the extension", {
  w <- tuneR::sine(440, duration=1000)
  f <- tempfile(fileext=".dat")
  writeAudio(w, f, mime="audio/x-wav")
  expect_true(file.exists(f))
  expect_equal(length(readAudio(f, mime="audio/x-wav")@left), 1000)
  unlink(f)
})

test_that("writeAudio rejects what it cannot write", {
  w <- tuneR::sine(440, duration=1000)
  expect_error(writeAudio(1, tempfile(fileext=".wav")), "Expecting a Wave or WaveMC object")
  expect_error(writeAudio(w, c("a.wav", "b.wav")), "file must be a single filename")
  expect_error(writeAudio(w, tempfile(fileext=".txt")), "Not an audio mime type")
  expect_error(writeAudio(w, tempfile(fileext=".notanextension")), "Could not determine the format")
})

test_that("writeAudio writes compressed formats", {
  skip_if_not_installed("av")
  w <- tuneR::sine(440, duration=4410, samp.rate=44100)
  w <- suppressWarnings(tuneR::normalize(w, unit="16"))

  f <- tempfile(fileext=".flac")
  writeAudio(w, f)
  expect_true(file.exists(f))
  r <- readAudio(f)
  expect_equal(r@samp.rate, 44100)
  #FLAC is lossless, so the audio must survive the round trip unchanged.
  expect_equal(r@bit, w@bit)
  expect_equal(r@left, w@left)
  unlink(f)

  f <- tempfile(fileext=".mp3")
  writeAudio(w, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
  unlink(f)
})

test_that("writeAudio reports what an encoder refused", {
  skip_if_not_installed("av")
  #Opus only accepts 48kHz and its divisors.
  w <- suppressWarnings(tuneR::normalize(tuneR::sine(440, duration=4410, samp.rate=44100), unit="16"))
  expect_error(writeAudio(w, tempfile(fileext=".opus")), "Could not write")
})

test_that("FLAC can be written without the av package", {
  skip_if(Sys.which("flac") == "", "the flac program is not installed")
  w <- suppressWarnings(tuneR::normalize(tuneR::sine(440, duration=4410, samp.rate=44100), unit="16"))
  f <- tempfile(fileext=".flac")
  expect_equal(.writeAudioFlac(w, f), f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
  unlink(f)
})

test_that("the flac program is found when it is not where seewave looks", {
  # seewave::wav2flac() runs whichever flac is on the PATH on unix, but on
  # Windows looks only in C:/Program Files/FLAC, so the flac Rtools ships went
  # unused and writing FLAC failed with "FLAC program was not found". The
  # arguments let each case be checked wherever the tests run.
  here <- normalizePath(tempdir(), winslash="/")
  program <- file.path(here, "flac.exe")

  # Away from Windows seewave finds the program itself.
  expect_equal(.flacExe(os="unix", expected=program, path=program), list())

  # On Windows, where seewave looks and the program is there.
  existing <- normalizePath(tempfile(fileext=".exe"), winslash="/", mustWork=FALSE)
  writeLines("", existing)
  on.exit(unlink(existing), add=TRUE)
  expect_equal(.flacExe(os="windows", expected=existing, path=program), list())

  # On Windows, where seewave looks and the program is not there, but the PATH
  # has one. This is the case that used to fail.
  missing <- file.path(here, "no-flac-here.exe")
  found <- .flacExe(os="windows", expected=missing, path=program)
  expect_named(found, c("path2exe", "exename"))
  expect_equal(found$path2exe, here)
  expect_equal(found$exename, "flac.exe")

  # On Windows with no flac anywhere, there is nothing to point seewave at and
  # it reports the program as missing, as it should.
  expect_equal(.flacExe(os="windows", expected=missing, path=""), list())
})

test_that("the flac program is looked for in the real places by default", {
  # The defaults are what the package actually uses, so they are checked to be
  # the places seewave and the system know about rather than only the arguments.
  defaults <- formals(.flacExe)
  expect_equal(defaults$expected, "C:/Program Files/FLAC/flac.exe")
  expect_equal(deparse(defaults$os), ".Platform$OS.type")
  expect_equal(deparse(defaults$path), "Sys.which(\"flac\")")
})

test_that("writeAudio round trips a multichannel wave through another format", {
  skip_if_not_installed("av")

  d <- matrix(0, nrow=4000, ncol=4)
  for (i in 1:4) {
    d[, i] <- round(16000 * sin(2 * pi * (220 * i) * seq_len(4000) / 8000))
  }
  colnames(d) <- c("FL", "FR", "FC", "BC")
  w <- tuneR::WaveMC(d, samp.rate=8000, bit=16)

  f <- tempfile(fileext=".flac")
  on.exit(unlink(f), add=TRUE)
  #The FLAC encoder prints a notice that it cannot record the channel layout of
  #a four channel file, which does not affect the samples it writes.
  expect_equal(writeAudio(w, f), f)

  r <- readAudio(f)
  expect_s4_class(r, "WaveMC")
  expect_equal(ncol(r@.Data), 4)
  expect_equal(r@samp.rate, 8000)
  expect_equal(r@bit, 16)
  expect_equal(unname(r@.Data), unname(d))
})

#A short tone to write out.
toneWave <- function(samp.rate=8000, n=4000) {
  tuneR::Wave(
    round(16000*sin(2*pi*440*seq_len(n)/samp.rate)),
    samp.rate=samp.rate, bit=16
  )
}

test_that("FLAC is written without the av package", {
  skip_if(
    !nzchar(Sys.which("flac")) && !file.exists("C:/Program Files/FLAC/flac.exe"),
    "the flac program is not installed"
  )
  asked <- list()
  local_mocked_bindings(
    package.installed = function(name, method=NULL, repo=NULL, askInstall=TRUE) {
      if (name != "av") {
        return(TRUE)
      }
      asked[[length(asked)+1]] <<- askInstall
      return(FALSE)
    }
  )

  f <- tempfile(fileext=".flac")
  withr::defer(unlink(f))
  expect_equal(writeAudio(toneWave(), f), f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
  #av was asked for, but only without a prompt: FLAC has another route to disk.
  expect_equal(asked, list(FALSE))
  #Reading it back would go through av, which is being held to be absent here, so
  #the file is identified by its own marker instead.
  expect_equal(rawToChar(readBin(f, "raw", n=4)), "fLaC")
})

test_that("a format with no other route to disk asks for av rather than writing nothing", {
  #package.installed() raises this itself when the installation is declined.
  local_mocked_bindings(
    package.installed = function(name, method=NULL, repo=NULL, askInstall=TRUE) {
      if (name != "av") {
        return(TRUE)
      }
      if (askInstall) {
        stop("Missing package not installed.")
      }
      return(FALSE)
    }
  )
  f <- tempfile(fileext=".mp3")
  withr::defer(unlink(f))
  expect_error(writeAudio(toneWave(), f), "Missing package not installed.")
  expect_false(file.exists(f))
})

test_that("av is asked for without a prompt first, and only then with one", {
  asked <- list()
  local_mocked_bindings(
    #Missing until it is asked for with a prompt, as it would be after installing.
    package.installed = function(name, method=NULL, repo=NULL, askInstall=TRUE) {
      if (name != "av") {
        return(TRUE)
      }
      asked[[length(asked)+1]] <<- askInstall
      return(askInstall)
    },
    .writeAudioAv = function(wave, file, ...) file
  )
  f <- tempfile(fileext=".mp3")
  withr::defer(unlink(f))
  expect_equal(writeAudio(toneWave(), f), f)
  #Asked quietly first, so a missing av only interrupts formats that need it.
  expect_equal(asked, list(FALSE, TRUE))
})

test_that("a FLAC conversion that produces nothing is reported", {
  local_mocked_bindings(
    wav2flac = function(file, overwrite=FALSE, ...) invisible(NULL),
    .package = "seewave"
  )
  f <- tempfile(fileext=".flac")
  withr::defer(unlink(f))
  expect_error(
    sonicscrewdriver:::.writeAudioFlac(toneWave(), f),
    "FLAC conversion failed, is the flac program installed?"
  )
})

test_that("a FLAC that cannot be put where it was asked for is reported", {
  #The conversion succeeds, but the destination is in a directory that is not
  #there, so neither moving nor copying the result can work.
  local_mocked_bindings(
    wav2flac = function(file, overwrite=FALSE, ...) {
      file.copy(file, paste0(tools::file_path_sans_ext(file), ".flac"))
      invisible(NULL)
    },
    .package = "seewave"
  )
  f <- file.path(tempdir(), "no-such-directory", "out.flac")
  expect_error(
    suppressWarnings(sonicscrewdriver:::.writeAudioFlac(toneWave(), f)),
    "Could not write to"
  )
})

test_that("the converted FLAC is moved to the path asked for", {
  converted <- NULL
  local_mocked_bindings(
    wav2flac = function(file, overwrite=FALSE, ...) {
      converted <<- paste0(tools::file_path_sans_ext(file), ".flac")
      #wav2flac() writes alongside its input, not where the caller wants it.
      file.copy(file, converted)
      invisible(NULL)
    },
    .package = "seewave"
  )
  f <- tempfile(fileext=".flac")
  withr::defer(unlink(f))
  expect_equal(sonicscrewdriver:::.writeAudioFlac(toneWave(), f), f)
  expect_true(file.exists(f))
  #Nothing is left behind in the temporary directory.
  expect_false(file.exists(converted))
})
