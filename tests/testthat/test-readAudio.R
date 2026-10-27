test_that("Reading files works", {
  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), input.unit="samples")
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

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from=48001, to=48000*2, input.unit="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from = 1, to = 2, input.unit= "seconds")
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

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), input.unit="samples")
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

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), mime="audio/mpeg", from = 1, to = 2, input.unit= "seconds")
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

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), input.unit="samples")
  expect_equal(length(w@left), 240000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), from=48001, to=48000*2, input.unit="samples")
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, FALSE)
  expect_equal(length(w@left), w@samp.rate)
  rm(w)

  w <- readAudio(system.file("extdata/AUDIOMOTH.flac", package="sonicscrewdriver"), from=1, to=2, input.unit="seconds")
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

  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"), from=1, to=2, input.unit="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.WAV", package="sonicscrewdriver"), from=48001, to=48000*2, input.unit="samples")
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

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"), from=1, to=2, input.unit="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"), from=48001, to=48000*2, input.unit="samples")
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
      length(readAudio(path, from=1/60, input.unit="minutes")@left),
      length(whole) - whole@samp.rate
    )
    expect_equal(
      length(readAudio(path, from=whole@samp.rate+1, input.unit="samples")@left),
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

    w <- readAudio(path, from=whole@samp.rate+1, to=length(whole)*2, input.unit="samples")
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
    expect_true(readAudio(path, from=1/60, input.unit="minutes")@stereo, label=file)
    expect_true(readAudio(path, from=48001, input.unit="samples")@stereo, label=file)

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
    r <- readAudio(f, from=101, to=200, input.unit="samples")
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), 100)
    expect_equal(unname(r@.Data), unname(w@.Data[101:200, ]))

    #In seconds, and in another time unit, which must land in the same place.
    r <- readAudio(f, from=0.5, to=1)
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), sr / 2)
    expect_equal(unname(r@.Data), unname(w@.Data[(sr / 2 + 1):sr, ]))
    expect_equal(readAudio(f, from=0.5 / 60, to=1 / 60, input.unit="minutes")@.Data, r@.Data)

    #A start with no end reads to the end of the file.
    r <- readAudio(f, from=0.5)
    expect_equal(ncol(r@.Data), 4)
    expect_equal(length(r), length(w) - sr / 2)
    expect_false(anyNA(r@.Data))
    expect_equal(unname(r@.Data), unname(w@.Data[(sr / 2 + 1):length(w), ]))
    expect_equal(length(readAudio(f, from=sr / 2 + 1, input.unit="samples")), length(w) - sr / 2)

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
                   readAudio(path, from=101, to=200, input.unit="samples"))) {
      expect_s4_class(w, "Wave")
      expect_false(inherits(w, "WaveMC"))
      expect_equal(w@stereo, startsWith(file, "STEREO"), label=file)
    }
  }
})

test_that(".mcChannelNames names channels by position", {
  expect_equal(.mcChannelNames(1), "FL")
  expect_equal(.mcChannelNames(4), c("FL", "FR", "FC", "LF"))
  expect_equal(.mcChannelNames(18), tuneR::MCnames$name)

  #More channels than tuneR has names for are numbered from where its names run
  #out, so the audio can still be read.
  n <- nrow(tuneR::MCnames)
  expect_equal(.mcChannelNames(n + 2), c(tuneR::MCnames$name, "C19", "C20"))
  expect_length(.mcChannelNames(n + 2), n + 2)
})

test_that(".avWaveMC refuses fewer samples than channels", {
  #matrix() would otherwise recycle a truncated frame into a full one, inventing
  #samples in the channels it did not reach.
  expect_error(.avWaveMC(1:3, 4, 8000, 16), "Fewer samples than channels")

  #A frame that is short of its last channels is dropped rather than filled in.
  w <- .avWaveMC(1:9, 4, 8000, 16)
  expect_s4_class(w, "WaveMC")
  expect_equal(length(w), 2)
  expect_equal(unname(w@.Data), matrix(1:8, ncol=4, byrow=TRUE))
})

# A WAV file assembled from its chunks, so that a header can be written that
# disagrees with the audio the file holds.
malformedWav <- function(samples, rate=48000, bits=16, format=1, dataSize=NULL,
                         riffSize=NULL) {
  blockAlign <- bits %/% 8
  audio <- if (format == 3) {
    writeBin(as.double(samples), raw(), size=blockAlign, endian="little")
  } else {
    writeBin(as.integer(samples), raw(), size=blockAlign, endian="little")
  }
  fmt <- c(
    charToRaw("fmt "), .uintToRaw(16, 4L),
    .uintToRaw(format, 2L), .uintToRaw(1, 2L), .uintToRaw(rate, 4L),
    .uintToRaw(rate * blockAlign, 4L), .uintToRaw(blockAlign, 2L),
    .uintToRaw(bits, 2L)
  )
  data <- c(
    charToRaw("data"),
    .uintToRaw(if (is.null(dataSize)) length(audio) else dataSize, 4L),
    audio
  )
  body <- c(charToRaw("WAVE"), fmt, data)
  file <- tempfile(fileext=".wav")
  writeBin(
    c(
      charToRaw("RIFF"),
      .uintToRaw(if (is.null(riffSize)) length(body) else riffSize, 4L),
      body
    ),
    file
  )
  return(file)
}

test_that("A file whose audio size was never written is read to the end of the file", {
  samples <- as.integer(round(sin(seq(0, 100, length.out=4800)) * 10000))
  f <- malformedWav(samples, dataSize=0, riffSize=8)
  on.exit(unlink(f))

  expect_warning(w <- readAudio(f), "was never written")
  expect_equal(length(w@left), length(samples))
  expect_equal(w@left, as.numeric(samples))
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)

  # Part of such a file can still be asked for.
  expect_warning(
    p <- readAudio(f, from=1, to=2400, input.unit="samples"),
    "was never written"
  )
  expect_equal(length(p@left), 2400)
  expect_equal(p@left, as.numeric(samples[1:2400]))

  # Reading the file must not change it.
  expect_equal(.wavChunksFile(f)$riffSize, 8)
})

test_that("A stereo recording is recovered a whole sample frame at a time", {
  n <- 1200
  w <- tuneR::Wave(
    left=round(sin(seq(0, 50, length.out=n)) * 9000),
    right=round(cos(seq(0, 50, length.out=n)) * 9000),
    samp.rate=44100, bit=16, pcm=TRUE
  )
  f <- tempfile(fileext=".wav")
  on.exit(unlink(f))
  tuneR::writeWave(w, f)

  bytes <- readBin(f, "raw", n=file.size(f))
  info <- .wavFormat(bytes)
  bytes[(info$dataStart - 4):(info$dataStart - 1)] <- raw(4)
  bytes[5:8] <- .uintToRaw(8, 4L)
  # Two stray bytes leave the audio short of a whole sample frame, which the
  # recovered size has to round down to.
  writeBin(c(bytes, as.raw(c(7, 7))), f)

  expect_warning(r <- readAudio(f), "was never written")
  expect_true(r@stereo)
  expect_equal(length(r@left), n)
  expect_equal(r@left, w@left)
  expect_equal(r@right, w@right)
})

test_that("A file that holds no audio at all is read as empty, not repaired", {
  f <- malformedWav(integer(0))
  on.exit(unlink(f))

  warnings <- character()
  w <- withCallingHandlers(
    readAudio(f),
    warning=function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(w@left), 0)
  expect_false(any(grepl("was never written", warnings)))
})

test_that("Samples that are not finite numbers are read as zero", {
  samples <- c(0.1, NaN, -0.2, Inf, 0.3, -Inf)
  f <- malformedWav(samples, bits=32, format=3)
  on.exit(unlink(f))

  expect_warning(w <- readAudio(f), "are not finite numbers")
  expect_equal(length(w@left), length(samples))
  expect_true(all(is.finite(w@left)))
  expect_equal(w@left[c(2, 4, 6)], c(0, 0, 0))
  expect_equal(w@left[c(1, 3, 5)], c(0.1, -0.2, 0.3), tolerance=1e-6)

  # A measurement of the wave is a number again.
  expect_false(is.nan(mean(w@left)))
})

test_that(".waveFinite replaces samples that are not finite numbers", {
  w <- tuneR::Wave(left=c(1, NaN), right=c(Inf, 2), samp.rate=8000, bit=16)
  expect_warning(r <- .waveFinite(w, "test.wav"), "are not finite numbers")
  expect_equal(r@left, c(1, 0))
  expect_equal(r@right, c(0, 2))

  m <- tuneR::WaveMC(
    data=matrix(c(1, NaN, Inf, 2, 3, 4), ncol=3), samp.rate=8000, bit=16
  )
  expect_warning(r <- .waveFinite(m, "test.wav"), "are not finite numbers")
  expect_equal(as.numeric(r@.Data), c(1, 0, 0, 2, 3, 4))

  expect_warning(
    .waveFinite(tuneR::Wave(left=c(1, NaN), samp.rate=8000, bit=16), "test.wav"),
    "is not a finite number"
  )

  # A wave that holds nothing but numbers is given back as it is, without a
  # warning.
  clean <- tuneR::Wave(left=c(1, 2), samp.rate=8000, bit=16)
  expect_identical(.waveFinite(clean, "test.wav"), clean)
})
