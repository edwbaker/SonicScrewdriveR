#Nothing here reaches Google. The two Google packages are stood in for, so what
#is checked is which of them is asked for what, and with which arguments.

#A transcript as gl_speech() returns one, with the times as strings of seconds.
fakeTranscript <- function(starts=c(1, 5), ends=c(2, 6)) {
  list(timings = data.frame(
    startTime = paste0(starts, "s"),
    endTime = paste0(ends, "s"),
    stringsAsFactors = FALSE
  ))
}

#A wave written to a temporary file, as the function reads its audio from disk.
waveFile <- function(samp.rate=8000, seconds=1, env=parent.frame()) {
  path <- tempfile(fileext=".wav")
  withr::defer(unlink(path), envir=env)
  t <- seq_len(samp.rate*seconds) / samp.rate
  wave <- tuneR::Wave(round(10000*sin(2*pi*440*t)), samp.rate=samp.rate, bit=16)
  tuneR::writeWave(wave, filename=path)
  return(path)
}

test_that("nothing is attempted without the Google packages", {
  local_mocked_bindings(package.installed = function(name, ...) FALSE)
  expect_null(gs_transcribe("no-such-file.wav"))
})

test_that("a file short enough to send directly is transcribed where it is", {
  path <- waveFile()
  local_mocked_bindings(
    package.installed = function(name, ...) TRUE,
    gs_transcribe_execute = function(object, ...) object
  )
  expect_equal(gs_transcribe(path), path)
})

test_that("a file above 48kHz is resampled before it is sent", {
  path <- waveFile(samp.rate=96000, seconds=1)
  sent <- NULL
  local_mocked_bindings(
    package.installed = function(name, ...) TRUE,
    gs_transcribe_execute = function(object, ...) {
      sent <<- object
      object
    }
  )
  expect_message(gs_transcribe(path), "Downsampling to 48kHz")
  expect_false(sent == path)

  #Resampled, not relabelled: the audio that arrives is at 48kHz and still lasts
  #as long as it did. Writing the original samples under a lower rate would have
  #slowed it down and dropped its pitch.
  resampled <- tuneR::readWave(sent)
  expect_equal(resampled@samp.rate, 48000)
  expect_equal(length(resampled@left)/48000, 1, tolerance=0.01)
  unlink(sent)
})

test_that("a file at or below 48kHz is sent as it is", {
  path <- waveFile(samp.rate=48000)
  local_mocked_bindings(
    package.installed = function(name, ...) TRUE,
    gs_transcribe_execute = function(object, ...) object
  )
  expect_no_message(expect_equal(gs_transcribe(path), path))
})

test_that("a long file is uploaded to the bucket and transcribed from there", {
  skip_if_not_installed("googleCloudStorageR")
  path <- waveFile()
  uploaded <- NULL
  deleted <- NULL
  local_mocked_bindings(
    package.installed = function(name, ...) TRUE,
    #A file long enough to need the bucket would take an hour to generate, so
    #its duration is the thing stood in for.
    duration = function(...) 4000,
    gs_transcribe_execute = function(object, ...) object
  )
  local_mocked_bindings(
    gcs_upload = function(file, bucket, name, ...) {
      uploaded <<- list(file=file, bucket=bucket, name=name)
      invisible(NULL)
    },
    gcs_delete_object = function(object, bucket, ...) {
      deleted <<- list(object=object, bucket=bucket)
      invisible(TRUE)
    },
    .package = "googleCloudStorageR"
  )

  expect_equal(gs_transcribe(path, bucket="my-bucket"), "gs://my-bucket/temp")
  expect_equal(uploaded$file, path)
  expect_equal(uploaded$bucket, "my-bucket")
  #The upload is tidied away again afterwards.
  expect_equal(deleted$object, "temp")
  expect_equal(deleted$bucket, "my-bucket")
})

test_that("a transcription that has finished is returned without waiting", {
  skip_if_not_installed("googleLanguageR")
  local_mocked_bindings(
    gl_speech = function(object, ...) structure(list(), class="gl_speech_op"),
    gl_speech_op = function(x, ...) fakeTranscript(),
    .package = "googleLanguageR"
  )
  res <- gs_transcribe_execute("gs://bucket/temp")
  #The transcript comes back with its times read as numbers.
  expect_equal(res$timings$startTime, c(1, 5))
  expect_equal(res$timings$endTime, c(2, 6))
})

test_that("the operation is asked again while it is still running", {
  skip_if_not_installed("googleLanguageR")
  calls <- 0
  local_mocked_bindings(
    gl_speech = function(object, ...) structure(list(), class="gl_speech_op"),
    gl_speech_op = function(x, ...) {
      calls <<- calls + 1
      if (calls < 3) {
        return(structure(list(), class="gl_speech_op"))
      }
      fakeTranscript(starts=2, ends=3)
    },
    .package = "googleLanguageR"
  )
  res <- gs_transcribe_execute("gs://bucket/temp", max.tries=10)
  expect_equal(calls, 3)
  expect_equal(res$timings$startTime, 2)
})

test_that("an operation that never finishes is given up on rather than looped forever", {
  skip_if_not_installed("googleLanguageR")
  local_mocked_bindings(
    gl_speech = function(object, ...) structure(list(), class="gl_speech_op"),
    gl_speech_op = function(x, ...) structure(list(), class="gl_speech_op"),
    .package = "googleLanguageR"
  )
  expect_error(
    gs_transcribe_execute("gs://bucket/temp", max.tries=2),
    "had not finished after 2 attempts"
  )
})

test_that("extra arguments are passed on to the speech request", {
  skip_if_not_installed("googleLanguageR")
  seen <- NULL
  local_mocked_bindings(
    gl_speech = function(object, ...) {
      seen <<- list(...)
      structure(list(), class="gl_speech_op")
    },
    gl_speech_op = function(x, ...) fakeTranscript(),
    .package = "googleLanguageR"
  )
  gs_transcribe_execute("gs://bucket/temp", languageCode="en-GB")
  expect_equal(seen$languageCode, "en-GB")
  expect_true(seen$asynch)
})
