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

#A four channel file is built here rather than shipped in inst/extdata, so that
#the tests do not carry a binary fixture for a case this is the only user of.
.fourChannel <- function(samp.rate=8000, seconds=2) {
  n <- samp.rate * seconds
  d <- matrix(0, nrow=n, ncol=4)
  for (i in 1:4) {
    d[, i] <- round(16000 * sin(2 * pi * (220 * i) * seq_len(n) / samp.rate))
  }
  colnames(d) <- c("FL", "FR", "FC", "BC")
  return(tuneR::WaveMC(d, samp.rate=samp.rate, bit=16))
}

test_that("readAudio returns a WaveMC for a file with more than two channels", {
  w <- .fourChannel()
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f), add=TRUE)
  writeAudio(w, f)

  r <- readAudio(f)
  expect_s4_class(r, "WaveMC")
  expect_equal(ncol(r@.Data), 4)
  expect_equal(length(r), length(w))
  expect_equal(r@samp.rate, w@samp.rate)
  expect_equal(r@bit, w@bit)
  #WAVE files are read by readWave() whatever the channel count, so the channel
  #names the file records survive the round trip.
  expect_equal(colnames(r@.Data), c("FL", "FR", "FC", "BC"))
  expect_equal(unname(r@.Data), unname(w@.Data))
})

test_that("readAudio reads a multichannel file through the av package", {
  skip_if_not_installed("av")

  w <- .fourChannel()
  wav <- tempfile(fileext=".wav")
  flac <- tempfile(fileext=".flac")
  on.exit(unlink(c(wav, flac)), add=TRUE)
  writeAudio(w, wav)
  #The FLAC encoder prints a notice that it cannot record the channel layout of
  #a four channel file, which does not affect the samples it writes.
  writeAudio(w, flac)

  r <- readAudio(flac)
  expect_s4_class(r, "WaveMC")
  expect_equal(ncol(r@.Data), 4)
  expect_equal(length(r), length(w))
  expect_equal(r@samp.rate, w@samp.rate)
  expect_equal(r@bit, 16)
  #FLAC is lossless, so the same audio read as WAVE and as FLAC must give the
  #same samples in the same channels despite taking different code paths.
  expect_equal(unname(r@.Data), unname(readAudio(wav)@.Data))
  #The channels of a wave read by the av package are named by position, as the
  #layout the av package reports does not identify them.
  expect_equal(colnames(r@.Data), tuneR::MCnames$name[1:4])
  #An unnamed WaveMC cannot be written back out, so the names matter.
  out <- tempfile(fileext=".wav")
  on.exit(unlink(out), add=TRUE)
  expect_equal(writeAudio(r, out), out)
})

test_that("readAudio keeps every channel of a multichannel file when cutting", {
  skip_if_not_installed("av")

  w <- .fourChannel()
  wav <- tempfile(fileext=".wav")
  flac <- tempfile(fileext=".flac")
  on.exit(unlink(c(wav, flac)), add=TRUE)
  writeAudio(w, wav)
  writeAudio(w, flac)

  for (f in c(wav, flac)) {
    sr <- w@samp.rate
    #In samples.
    r <- readAudio(f, from=101, to=200, units="samples")
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), 100)
    expect_equal(unname(r@.Data), unname(w@.Data[101:200, ]))

    #In seconds, and in another time unit, which must land in the same place.
    r <- readAudio(f, from=0.5, to=1)
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), sr / 2)
    expect_equal(unname(r@.Data), unname(w@.Data[(sr / 2 + 1):sr, ]))
    expect_equal(readAudio(f, from=0.5 / 60, to=1 / 60, units="minutes")@.Data, r@.Data)

    #A start with no end reads to the end of the file.
    r <- readAudio(f, from=0.5)
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), length(w) - sr / 2)
    expect_false(anyNA(r@.Data))
    expect_equal(unname(r@.Data), unname(w@.Data[(sr / 2 + 1):length(w), ]))
    expect_equal(length(readAudio(f, from=sr / 2 + 1, units="samples")), length(w) - sr / 2)

    #As does an end point beyond it.
    expect_equal(length(readAudio(f, from=0.5, to=3600)), length(w) - sr / 2)
  }
})

test_that("readAudio cuts a multichannel file where it cuts a stereo one", {
  skip_if_not_installed("av")

  #cutw() cannot take a WaveMC, so a multichannel wave is cut by row instead.
  #The two have to cut at the same sample, or the units asked for would change
  #the section a recording gives depending on its channel count.
  w <- .fourChannel()
  mc <- tempfile(fileext=".flac")
  st <- tempfile(fileext=".flac")
  on.exit(unlink(c(mc, st)), add=TRUE)
  writeAudio(w, mc)
  two <- w@.Data[, 1:2]
  colnames(two) <- c("FL", "FR")
  writeAudio(tuneR::WaveMC(two, samp.rate=w@samp.rate, bit=w@bit), st)

  for (args in list(list(from=0.3, to=1.1), list(from=0.3), list(from=0.25, to=0.75))) {
    a <- do.call(readAudio, c(list(mc), args))
    b <- do.call(readAudio, c(list(st), args))
    expect_equal(length(a), length(b))
    expect_equal(as.vector(a@.Data[, 1]), b@left)
    expect_equal(as.vector(a@.Data[, 2]), b@right)
  }
})

test_that("readAudio returns a Wave for mono and stereo files", {
  #Reading one and two channel files must not start returning a WaveMC, which
  #would change the result of every call that has ever been made.
  files <- c("AUDIOMOTH.WAV", "AUDIOMOTH.mp3", "AUDIOMOTH.flac",
             "STEREO.WAV", "STEREO.mp3", "STEREO.flac")
  for (file in files) {
    path <- system.file("extdata", file, package="sonicscrewdriver")
    for (w in list(readAudio(path), readAudio(path, from=0.5),
                   readAudio(path, from=0.5, to=1),
                   readAudio(path, from=101, to=200, units="samples"))) {
      expect_s4_class(w, "Wave")
      expect_false(inherits(w, "WaveMC"))
      expect_equal(w@stereo, startsWith(file, "STEREO"), label=file)
    }
  }
})
