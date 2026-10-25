test_that("Reading files works", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), mime="audio/x-wav")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from=48001, to=48000*2, units="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from = 1, to = 2, units = "seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"))
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), mime="audio/mpeg")
  expect_equal(length(w@left), 241920)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), mime="audio/mpeg", from = 1, to = 2, units = "seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), units="samples")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), from=48001, to=48000*2, units="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), from=1, to=2, units="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"), from=1, to=2, units="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"), from=48001, to=48000*2, units="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"), from=1, to=2, units="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"), from=48001, to=48000*2, units="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  # TODO: empty.wav

})

test_that("readAudio reads to the end of the file given a start and no end", {
  #to defaults to Inf, which seewave::cutw() rejects as an end point past the
  #end of the recording, so a start with no end used to fail for every format
  #not read by readWave().
  files <- c("AUDIOMOTH.WAV", "AUDIOMOTH.mp3", "AUDIOMOTH.flac",
             "STEREO.WAV", "STEREO.mp3", "STEREO.flac")
  for (file in files) {
    path <- system.file("extdata", file, package="sonicscrewdriver")
    whole <- readAudio(path)
    expect_equal(length(readAudio(path, from=1)@left), length(whole) - whole@samp.rate)
    expect_equal(
      length(readAudio(path, from=1/60, units="minutes")@left),
      length(whole) - whole@samp.rate
    )
    expect_equal(
      length(readAudio(path, from=whole@samp.rate+1, units="samples")@left),
      length(whole) - whole@samp.rate
    )
  }
})

test_that("readAudio reads to the end of the file given an end beyond it", {
  files <- c("AUDIOMOTH.WAV", "AUDIOMOTH.mp3", "AUDIOMOTH.flac",
             "STEREO.WAV", "STEREO.mp3", "STEREO.flac")
  for (file in files) {
    path <- system.file("extdata", file, package="sonicscrewdriver")
    whole <- readAudio(path)
    #An end point past the end of the recording gives the audio that is there,
    #rather than an error or samples padded with NA.
    w <- readAudio(path, from=1, to=3600)
    expect_equal(length(w@left), length(whole) - whole@samp.rate)
    expect_false(anyNA(w@left))

    w <- readAudio(path, from=whole@samp.rate+1, to=length(whole)*2, units="samples")
    expect_equal(length(w@left), length(whole) - whole@samp.rate)
    expect_false(anyNA(w@left))
  }
})

test_that("readAudio gives the same audio from a start with no end however it was read", {
  skip_if_not_installed("av")

  #AUDIOMOTH.flac is a lossless copy of AUDIOMOTH.WAV, so cutting the two at the
  #same point must give the same samples despite taking different code paths.
  wav <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from=1)
  flac <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), from=1)
  expect_equal(flac@left, wav@left)
})

test_that("readAudio keeps every channel however the cut was asked for", {
  #seewave::cutw() returns one channel per call, so a stereo file cut in time
  #units used to come back mono while the same cut in samples kept both.
  for (file in c("STEREO.WAV", "STEREO.mp3", "STEREO.flac")) {
    path <- system.file("extdata", file, package="sonicscrewdriver")
    expect_true(readAudio(path)@stereo, label=file)
    expect_true(readAudio(path, from=1)@stereo, label=file)
    expect_true(readAudio(path, from=1, to=2)@stereo, label=file)
    expect_true(readAudio(path, from=1/60, units="minutes")@stereo, label=file)
    expect_true(readAudio(path, from=48001, units="samples")@stereo, label=file)

    #The right channel has to be the right channel, not a copy of the left.
    whole <- readAudio(path)
    cut <- readAudio(path, from=1)
    expect_equal(cut@right, utils::tail(whole@right, length(cut@right)), label=file)
  }
})

test_that("readAudio still rejects a negative end point", {
  #Only a positive infinite end point means "to the end of the file"; -Inf is a
  #negative time and is still an error.
  for (file in c("AUDIOMOTH.mp3", "AUDIOMOTH.flac")) {
    path <- system.file("extdata", file, package="sonicscrewdriver")
    expect_error(readAudio(path, from=1, to=-Inf), "cannot be negative")
    expect_error(readAudio(path, from=1, to=-2), "cannot be negative")
  }
})

test_that("readAudio rejects bad files", {
  expect_error(readAudio(system.file("extdata/CONFIG.TXT", package="sonicscrewdriver")), "Could not determine number of channels.")
})

test_that("audio read by the av package matches the same audio read as WAVE", {
  skip_if_not_installed("av")

  #AUDIOMOTH.flac is a lossless copy of AUDIOMOTH.WAV, so the two must give the
  #same samples at the same bit depth despite taking different code paths.
  wav <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
  flac <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))
  expect_equal(flac@bit, wav@bit)
  expect_equal(flac@samp.rate, wav@samp.rate)
  expect_equal(flac@left, wav@left)
})

test_that("readAudio does not leave av attributes on the samples", {
  skip_if_not_installed("av")

  #read_audio_bin() attaches sample_rate and channels to what it returns.
  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"))
  expect_null(attributes(w@left))
  expect_null(attributes(w@right))

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"))
  expect_null(attributes(w@left))
  expect_null(attributes(w@right))
})

test_that(".avBitdepth gives correct output", {
  expect_equal(.avBitdepth("u8", 1), 8)
  expect_equal(.avBitdepth("s16", 1), 16)
  expect_equal(.avBitdepth("s16p", 1), 16)
  expect_equal(.avBitdepth("s32", 1), 32)
  expect_equal(.avBitdepth("flt", 1), 32)
  expect_equal(.avBitdepth("fltp", 1), 32)
  expect_equal(.avBitdepth("dbl", 1), 32)

  #An unusable sample format falls back to inferring the depth from the samples.
  expect_equal(.avBitdepth(NULL, 2^16/2), 16)
  expect_equal(.avBitdepth(NA_character_, 2^16/2), 16)
  expect_equal(.avBitdepth("notaformat", 2^16/2), 16)
})

test_that(".bitdepth gives correct output", {
  expect_equal(.bitdepth(2^8/2), 8)
  expect_equal(.bitdepth(2^8/2+1), 16)
  expect_equal(.bitdepth(2^16/2), 16)
  expect_equal(.bitdepth(2^16/2+1), 24)
  expect_equal(.bitdepth(2^24/2), 24)
  expect_equal(.bitdepth(2^24/2+1), 32)
  expect_equal(.bitdepth(2^32/2), 32)
  expect_error(.bitdepth(2^64/2+1), "Bit depths above 32bit are not supported.")
})
