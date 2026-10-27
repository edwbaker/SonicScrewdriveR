#' Calculated size of raw audio files
#'
#' Calculates the raw size of audio date at set sample rate, bit depth and duration.
#'
#' The output is in bits unless `output.unit` is set to "bytes", or to "human"
#' for a human readable size from `humanBytes()`.
#'
#' @param samp.rate Sample rate
#' @param bit.depth Bit depth
#' @param channels The number of audio channels
#' @param duration Duration of recording
#' @param duration.unit The unit the duration is given in, one of "seconds",
#'   "minutes", "hours", "days", "years", "HHMM", "POSIX".
#' @param output.unit "bits", "bytes" or "human"
#' @export
#' @return The size of the audio file in the specified unit
#' @examples
#' # One minute of mono 16-bit audio sampled at 44.1kHz
#' audio_filesize(samp.rate=44100, bit.depth=16, channels=1, duration=1, duration.unit="minutes")
#'
#' # One year of stereo 24-bit audio sampled at 96kHz
#' audio_filesize(samp.rate=96000, bit.depth=24, channels=2, duration=1, duration.unit="years")
#'
audio_filesize <- function(samp.rate=44100, bit.depth=16, channels=1, duration=1, duration.unit="seconds", output.unit="bits") {
  output.unit <- .validateUnit(
    output.unit, c("bits", "bytes", "human"), "audio_filesize", "output.unit"
  )
  duration <- convert2seconds(duration, input.unit=duration.unit)
  bits <- samp.rate * bit.depth * duration * channels
  return(switch(
    output.unit,
    "bits"  = bits,
    "bytes" = convert2bytes(bits, "bits"),
    "human" = humanBytes(convert2bytes(bits, "bits"))
  ))
}
