#' Read an audio file
#'
#' This file is used to read an audio file and return a Wave object, it is an abstraction
#' function for various specific audio reading functions. If no existing method can be identified
#' an attempt is made to use the av package to read the audio.
#'
#' @details
#' Files read through the av package (which is everything that is not WAVE or
#' MP3, e.g. FLAC) are returned at the bit depth the file was stored at, so that
#' the same audio compares equal however it was read. The av package does not
#' report a bit depth of 24, and returns 24bit audio as 32bit, so the samples of
#' a 24bit file are 256 times those `tuneR::readWave()` would give for the same
#' audio stored as WAVE. Audio stored as floating point is returned as 32bit.
#'
#' @param file File to read
#' @param mime MIME type of file to read, or "auto". Supported types are "audio/x-wav" and "audio/mpeg" (MP3)
#' @param from Start point in file to return
#' @param to End point in file to return
#' @param units One of "samples", "seconds", "minutes", "hours". Default is "seconds".
#' @return A Wave object
#' @export
#' @importFrom tuneR readMP3 readWave stereo Wave
#' @importFrom seewave cutw
#' @importFrom tools file_ext
#' @importFrom mime guess_type
readAudio <- function(file, mime="auto", from=0, to=Inf, units="seconds") {
  if (mime == "auto") {
    mime <- guess_type(file)
  }
  if (units=="samples" & from == 0) {
    fromS <- 1
  } else{
    fromS <- from
  }

  if (mime == "audio/x-wav") {
    tryCatch({
      wave <- readWave(file, from=fromS, to=to, units=units)
      return(wave)
    },
    error=function(cond){

    })
  }

  if (mime=="audio/mpeg") {
    wave <- NULL
    wave <- tryCatch({
      wave <- readMP3(file)
      if (units=="samples") {
        return(cutws(wave, fromS, to))
      }
      if (from==0 && to==Inf) {
        return(wave)
      }
      from <- convert2seconds(from, input=units)
      to <- convert2seconds(to, input=units)
      return(cutw(wave,from=from, to=to, output="Wave"))
    },
    error=function(cond){
    })
  }

  #Check if av package available
  if (package.installed("av", askInstall=TRUE)) {
    #Using av package
    info <- av::av_media_info(file)$audio
    channels <- info[['channels']]
    if (is.null(channels)) {
      stop("Could not determine number of channels.")
    }
    if (channels > 2) {
      stop("channel count greater than 2 is not supported")
    }

    samples <- av::read_audio_bin(file, channels=channels)
    samp.rate <- attr(samples, "sample_rate")
    if (is.null(samp.rate)) {
      samp.rate <- info[['sample_rate']]
    }
    #read_audio_bin() attaches sample_rate and channels to the vector it returns,
    #and they would be carried into the slots of the Wave object if left on it.
    samples <- as.vector(samples)
    samples[which(is.na(samples))] <- 0

    #read_audio_bin() fills the 32bit range whatever the file was stored at, so
    #the samples are scaled back down to the bit depth of the source. Without
    #this the same audio read as WAVE and as FLAC does not compare equal.
    bit <- .avBitdepth(info[['sample_fmt']], samples)
    samples <- samples / 2^(32 - bit)
    if (bit == 8) {
      #8bit PCM is unsigned, which is how readWave() returns it.
      samples <- samples + 128
    }

    if (channels == 1) {
      wave <- Wave(left=samples, samp.rate=samp.rate, bit=bit)
    }
    if (channels == 2) {
      left <- samples[seq(1, length(samples), by = 2)]
      right <- samples[seq(2, length(samples), by = 2)]
      wave <- Wave(left=left, right=right, samp.rate=samp.rate, bit=bit)
    }

    if (units == "samples") {
      return(cutws(wave,from=fromS, to=to))
    } else {
      if (from==0 & to == Inf) {
        return(wave)
      }
      if (channels == 1) {
        return(cutw(wave, from=convert2seconds(from, units), to=convert2seconds(to, units), output="Wave"))
      }
      if (channels == 2) {
        left <- cutw(wave, channel=1, from=convert2seconds(from, units), to=convert2seconds(to, units), output="Wave")
        right <- cutw(wave, channel=2, from=convert2seconds(from, units), to=convert2seconds(to, units), output="Wave")
        return(stereo(left, right))
      }
    }
  }
  stop("File could not be processed")
}

.bitdepth <- function(v) {
  m <- ceiling(max(abs(v), na.rm=TRUE))
  if (m <= 128) { return(8) }
  if (m <= 32768) { return(16) }
  if (m <= 8388608) { return(24) }
  if (m <= 2147483648) { return(32) }
  stop("Bit depths above 32bit are not supported.")
}

#' Bit depth of the audio decoded by the av package
#'
#' av::read_audio_bin() returns samples filling the 32bit range whatever the
#' source was stored at, so the bit depth has to come from the sample format
#' the av package reports for the file. Note that the av package does not
#' expose bits_per_raw_sample, and ffmpeg decodes 24bit audio into a 32bit
#' sample format, so a 24bit source is reported here as 32bit. The samples are
#' exact multiples of 256 in that case, so nothing is lost, but the values are
#' 256 times those readWave() would give for the same audio stored as WAVE.
#'
#' @param sample_fmt The sample_fmt given by av::av_media_info().
#' @param samples The decoded samples, used only if sample_fmt is unusable.
#' @return A bit depth, one of 8, 16 or 32.
#' @keywords internal
#' @noRd
.avBitdepth <- function(sample_fmt, samples) {
  if (length(sample_fmt) != 1 || is.na(sample_fmt)) {
    return(.bitdepth(samples))
  }
  #Planar formats hold each channel in its own buffer, but describe the same
  #sample depth as their interleaved counterparts.
  fmt <- sub("p$", "", as.character(sample_fmt))
  bit <- switch(
    fmt,
    "u8" = 8,
    "s16" = 16,
    #Floating point audio has no bit depth of its own, and read_audio_bin() has
    #already made 32bit integers of it.
    "s32" = 32,
    "s64" = 32,
    "flt" = 32,
    "dbl" = 32,
    NULL
  )
  if (is.null(bit)) {
    return(.bitdepth(samples))
  }
  return(bit)
}
