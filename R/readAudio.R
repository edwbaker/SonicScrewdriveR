#' Read an audio file
#'
#' This file is used to read an audio file and return a Wave object, it is an abstraction
#' function for various specific audio reading functions. If no existing method can be identified
#' an attempt is made to use the av package to read the audio.
#'
#' @param file File to read
#' @param mime MIME type of file to read, or "auto". Supported types are "audio/x-wav" and "audio/mpeg" (MP3)
#' @param from Start point in file to return
#' @param to End point in file to return
#' @param units One of "samples", "seconds", "minutes", "hours"
#' @return A Wave object
#' @export
#' @importFrom tuneR readMP3 readWave Wave
#' @importFrom seewave cutw
#' @importFrom tools file_ext
#' @importFrom mime guess_type
readAudio <- function(file, mime="auto", from=1, to=Inf, units="samples") {
  if (mime == "auto") {
    mime <- guess_type(file)
  }

  if (mime == "audio/x-wav") {
    wave <- readWave(file, from=from, to=to, units=units)
    return(wave)
  }

  if (mime=="audio/mpeg") {
    #Check if av package available
    if (package.installed("av", askInstall=FALSE)==FALSE) {

      wave <- readMP3(file)
      if (from==1 && to==Inf) {
        return(wave)
      }

      if (units=="samples") {
        return(cutws(wave, from, to))
      }

      from <- convert2seconds(from, input=units)
      to <- convert2seconds(from, input=units)
      return(cutw(wave,from=from, to=to))
    }
    #av available - continue
  } else {
    message("Unidentified MIME type - attempting to read using package av")
    package.installed("av")
  }

  #Using av package
  channels <- av::av_media_info(file)$audio[['channels']]
  if (channels > 2) {
    stop("channel count greater than 2 is not supported")
  }

  if (from==1 && to==Inf) {
    wave <- av::read_audio_bin(file, channels=channels)
  } else {
    if (units != "samples") {
      from <- convert2seconds(from, input=units)
      to <- convert2seconds(from, input=units)
      wave <- av::read_audio_bin(file, channels=1, start_time=from, end_time=to)
    }
  }

  wave[which(is.na(wave))] <- 0
  bit <- bitdepth(wave)

  if (channels == 1) {
    wave <- Wave(left=wave, samp.rate=attr(wave, "sample_rate"), bit=bit)
    return(wave)
  }
  if (channels == 2) {
    left <- wave[seq(1, length(wave), by = 2)]
    right <- wave[seq(2, length(wave), by = 2)]
    wave <- Wave(left=left, right=right, samp.rate=attr(wave, "sample_rate"), bit=bit)
    d <- 2
    return(wave)
  }

  if (units == "samples") {
    return(cutws(wave,from=from, to=to))
  } else {
    return(wave)
  }

  stop("File could not be processed")
}

bitdepth <- function(v) {
  m <- max(abs(v), na.rm=TRUE)
  if (m <= 128) { return(8) }
  if (m <= 32768) { return(16) }
  if (m <= 2147483648) { return(32) }
  if (m <= 9223372036854775808) { return(64) }
  stop("Bit depths above 64bit are not supported")
}
