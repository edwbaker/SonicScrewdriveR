#' Calculated size of raw audio files
#'
#' Calculates the raw size of audio date at set sample rate, bit depth and duration.
#'
#' @param samp.rate Sample rate
#' @param bit.depth Bit depth
#' @param duration Duration of recording
#' @param duration.unit One of seconds, minutes, hours, days.
#' @export
#'

audio_filesize <- function(samp.rate=44100, bit.depth=16, duration=1, duration.unit="seconds") {
  if (duration.unit == "minutes") {
    duration <- duration * 60
  }
  if (duration.unit == "hours") {
    duration <- duration * 60 * 60
  }
  if (duration.unit == "days") {
    duration <- duration * 60 * 60 * 24
  }
    return (samp.rate * bit.depth * duration)
}
