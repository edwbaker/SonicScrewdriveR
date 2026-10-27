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

  w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"), from = 1, to = 2, input.unit="seconds")
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

  w <- readAudio(system.file("extdata/AUDIOMOTH.mp3", package="sonicscrewdriver"), mime="audio/mpeg", from = 1, to = 2, input.unit="seconds")
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
  expect_equal(w@bit, 32)
  expect_equal(w@pcm, TRUE)
  expect_equal(w@stereo, TRUE)
  rm(w)

  w <- readAudio(system.file("extdata/STEREO.flac", package="sonicscrewdriver"), from=1, to=2, input.unit="seconds")
  expect_equal(length(w@left), 48000)
  expect_equal(length(w@left), 48000)
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 32)
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

test_that("readAudio rejects bad files", {
  expect_error(readAudio(system.file("extdata/CONFIG.TXT", package="sonicscrewdriver")), "Could not determine number of channels.")
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

# A WAV file assembled from its chunks, so that a header can be written that
# disagrees with the audio the file holds.
wavFile <- function(samples, rate=48000, bits=16, format=1, dataSize=NULL,
                    riffSize=NULL, extra=raw(0)) {
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
  body <- c(charToRaw("WAVE"), fmt, data, extra)
  bytes <- c(
    charToRaw("RIFF"),
    .uintToRaw(if (is.null(riffSize)) length(body) else riffSize, 4L),
    body
  )
  file <- tempfile(fileext=".wav")
  writeBin(bytes, file)
  return(file)
}

# The metadata chunk a recorder writes after the audio.
guanChunk <- function(text) {
  body <- charToRaw(text)
  if (length(body) %% 2L == 1L) {
    body <- c(body, charToRaw(" "))
  }
  return(c(charToRaw("guan"), .uintToRaw(length(body), 4L), body))
}

test_that("A file whose audio size was never written is read to the end of the file", {
  samples <- as.integer(round(sin(seq(0, 100, length.out=4800)) * 10000))
  f <- wavFile(samples, dataSize=0, riffSize=8)
  on.exit(unlink(f))

  expect_warning(w <- readAudio(f), "was never written")
  expect_equal(length(w@left), length(samples))
  expect_equal(w@left, as.numeric(samples))
  expect_equal(w@samp.rate, 48000)
  expect_equal(w@bit, 16)

  # Part of such a file can still be asked for.
  expect_warning(p <- readAudio(f, from=1, to=2400, input.unit="samples"), "was never written")
  expect_equal(length(p@left), 2400)
  expect_equal(p@left, as.numeric(samples[1:2400]))

  # Reading the file must not change it.
  expect_equal(.wavFormat(readBin(f, "raw", n=file.size(f)))$riffSize, 8)
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
  f <- wavFile(integer(0), extra=guanChunk("GUANO|Version:1.0
Length:0"))
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
  expect_equal(guano(f)[["Length"]], 0)
})

test_that("Samples that are not finite numbers are read as zero", {
  samples <- c(0.1, NaN, -0.2, Inf, 0.3, -Inf)
  f <- wavFile(samples, bits=32, format=3)
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

  m <- tuneR::WaveMC(data=matrix(c(1, NaN, Inf, 2, 3, 4), ncol=3), samp.rate=8000, bit=16)
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

test_that("A chunk header is told from audio the writer never recorded", {
  bytes <- c(charToRaw("guan"), .uintToRaw(10, 4L), raw(10))
  expect_true(.wavIsChunkHeader(bytes, 1L, length(bytes)))
  # A chunk that could not fit in the file is not one.
  expect_false(.wavIsChunkHeader(bytes, 1L, 8L))
  # Audio rarely begins with four printable characters.
  expect_false(.wavIsChunkHeader(c(as.raw(c(0, 255, 7, 200)), .uintToRaw(10, 4L)), 1L, 18L))
  # There is nothing to read past the end of what was read.
  expect_false(.wavIsChunkHeader(bytes, 12L, 1000L))
})
