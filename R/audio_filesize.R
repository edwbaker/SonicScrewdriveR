#' Calculated size of raw audio files
#'
#' Calculates the raw size of audio date at set sample rate, bit depth and duration.
#'
#' By default `humanBytes()` is used to convert the output to human readable format,
#' however this can be changed by setting `output.unit` to "bits" or "bytes".
#'
#' @param samp.rate Sample rate
#' @param bit.depth Bit depth
#' @param channels The number of audio channels
#' @param duration Duration of recording
#' @param duration.unit One of seconds, minutes, hours, days
#' @param output.unit "human", "bits" or  "bytes"
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
  duration <- convert2seconds(duration, duration.unit)
  bits <- samp.rate * bit.depth * duration * channels
  if (output.unit == "bits") {
    return (bits)
  } else if (output.unit == "bytes") {
    return (convert2bytes(bits, "bits"))
  } else if (output.unit == "human") {
    return (humanBytes(convert2bytes(bits, "bits")))
  } else {
    stop(paste("Unknown output unit:", output.unit))
  }
}
