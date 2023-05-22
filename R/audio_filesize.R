#' Calculated size of raw audio files
#'
#' Calculates the raw size of audio date at set sample rate, bit depth and duration.
#'
#' @param samp.rate Sample rate
#' @param bit.depth Bit depth
#' @param channels The number of audio channels
#' @param duration Duration of recording
#' @param duration.unit One of seconds, minutes, hours, days
#' @param output.unit "bits" or  "bytes"
#' @export
#'

audio_filesize <- function(samp.rate=44100, bit.depth=16, channels=1, duration=1, duration.unit="seconds", output.unit="bits") {
  duration <- convert2seconds(duration, duration.unit)
  bits <- samp.rate * bit.depth * duration * channels
  if (output.unit == "bits") {
    return (bits)
  }
  if (output.unit == "bytes") {
    return (convert2bytes(bits, "bits"))
  }
}
