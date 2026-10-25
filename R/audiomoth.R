#' Read AudioMoth configuration file
#'
#' Reads and parses an AudioMoth configuration file.
#'
#' @param filename Path to the configuration file to read
#' @return A data frame of the configuration settings, with columns Key and Value
#' @export
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' audiomothConfig("./CONFIG.TXT")
#' }
#'
audiomothConfig <- function(filename) {
  f <- readLines(filename)
  c <- read.csv(textConnection(sub(":", "|", f)), header=FALSE, sep="|")
  #Both columns. Replacing the colon with a separator leaves a space at the front
  #of every value, so comparing a value against a string never matched.
  c[] <- lapply(c, trimws)
  colnames(c) <- c("Key", "Value")
  return(c)
}

#' Capture groups from an AudioMoth comment
#'
#' @param x The comment string to match against
#' @param pattern A regular expression containing at least one capture group
#' @return The capture groups of the first match, or NULL if there is no match
#' @noRd
audiomothMatch <- function(x, pattern) {
  m <- regmatches(x, regexec(pattern, x, perl=TRUE))[[1]]
  if (length(m) < 2) {
    return(NULL)
  }
  return(m[-1])
}

#' A single field from an AudioMoth comment
#'
#' @param x The comment string to match against
#' @param pattern A regular expression containing at least one capture group
#' @return The first capture group of the first match, or NA if there is no match
#' @noRd
audiomothField <- function(x, pattern) {
  m <- audiomothMatch(x, pattern)
  if (is.null(m)) {
    return(NA_character_)
  }
  return(m[1])
}

#' Read AudioMoth metadata from a wave file
#'
#' Reads and parses metadata stored in wave files produced by
#' AudioMoth devices.
#'
#' The comment written into the wave header has been reworded several times
#' across AudioMoth-Firmware-Basic releases, most significantly in 1.6.0, where
#' "at medium gain setting while battery state was" became "at medium gain while
#' battery was" and the filter and cancellation clauses were rephrased. Fields
#' are matched by pattern rather than by position, so all wordings from 1.0
#' onwards are read.
#'
#' Firmware from 1.5.0 can be configured with a deployment ID, which the
#' firmware writes in place of the device serial number. Such files have a
#' \code{deployment_id} and no \code{serial}.
#'
#' Firmware before 1.4.0 wrote a gain index rather than a gain name, so
#' \code{gain} is a number ("0" to "4") for files from those versions.
#'
#' @param filename Path to the wave file to read
#' @return A list of extracted parameters:
#' \describe{
#'   \item{raw}{The comment string the other fields were read from}
#'   \item{start_time}{Recording start time, as \code{"HH:MM:SS"}}
#'   \item{start_date}{Recording start date, as \code{"DD/MM/YYYY"}}
#'   \item{time_zone}{Time zone of the start time, e.g. \code{"UTC"} or
#'     \code{"UTC+1"}}
#'   \item{serial}{The device serial number, or NA for a deployment ID file}
#'   \item{deployment_id}{The configured deployment ID, or NA if none was set}
#'   \item{gain}{Gain setting, one of "low", "low-medium", "medium",
#'     "medium-high" or "high"}
#'   \item{voltage}{Battery voltage in volts. Where the firmware reported only
#'     that the battery was outside the range it measures, the threshold it
#'     compared against is returned with a leading \code{"<"} or \code{">"},
#'     e.g. \code{"<2.5"}. The thresholds themselves have changed: firmware up
#'     to 1.2.0 wrote \code{"<3.6"} and \code{">5.0"}, 1.2.2 and 1.3.0
#'     \code{"<3.6"} and \code{">4.9"}, and 1.4.0 onwards \code{"<2.5"} and
#'     \code{">4.9"}}
#'   \item{temperature}{Temperature in degrees Celsius, or NA for firmware
#'     before 1.4.0, which did not record it}
#'   \item{external_mic}{Whether an external microphone was used}
#'   \item{filter}{"Low-pass", "Band-pass", "High-pass", or FALSE if no filter
#'     was applied}
#'   \item{filter.limit}{Filter frequency in kHz, two values separated by a
#'     hyphen for a band-pass filter, or FALSE if no filter was applied}
#'   \item{cancelled}{Why the recording stopped early, or FALSE if it ran to
#'     completion}
#' }
#' @export
#' @examples
#' \dontrun{
#' audiomothWave("./FILENAME.WAV")
#' }
#'
audiomothWave <- function(filename) {
  f <- readBin(filename, "character", n=60L)
  raw <- f[grep("Recorded", f, useBytes = TRUE)]

  if (length(raw)==0) {
    return(list())
  }
  raw <- raw[1]

  start_time <- audiomothField(raw, "([0-2][0-9]:[0-6][0-9]:[0-6][0-9])")
  start_date <- audiomothField(raw, "([0-3][0-9]/[0-1][0-9]/[0-9]{4})")
  time_zone <- audiomothField(raw, "\\((UTC[^)]*)\\)")

  # The firmware writes either the device serial number ("by AudioMoth <id>")
  # or, where a deployment ID has been configured, that ID in its place
  # ("during deployment <id>"). Only one of the two is ever present.
  serial <- audiomothField(raw, "by AudioMoth ([0-9A-F]{16})")
  deployment_id <- audiomothField(raw, "during deployment ([0-9A-F]{16})")

  # Up to 1.5.0 the gain was followed by "gain setting while battery state
  # was", from 1.6.0 by "gain while battery was". Before 1.4.0 the gain was
  # written as an index instead of a name.
  gain <- audiomothField(raw, "at (low-medium|medium-high|low|medium|high) gain")
  if (is.na(gain)) {
    gain <- audiomothField(raw, "at gain setting ([0-9]+) while")
  }

  # Where the battery was outside the range the firmware measures, the state is
  # written as a comparison against a threshold, and both the wording and the
  # threshold have changed: 1.0 to 1.2.0 wrote "< 3.6V" and "> 5.0V", 1.2.2 and
  # 1.3.0 "less than 3.6V" and "greater than 4.9V", and 1.4.0 onwards "less
  # than 2.5V" and "greater than 4.9V". Reading the comparator separately from
  # the threshold keeps every one of them. Firmware 1.0 wrote the measured
  # voltage with two decimal places, the rest with one.
  m <- audiomothMatch(
    raw,
    "battery(?: state)? was (less than|greater than|<|>) ?([0-9]+\\.[0-9]{1,2})V"
  )
  if (!is.null(m)) {
    voltage <- paste0(
      if (is.element(m[1], c("less than", "<"))) "<" else ">",
      m[2]
    )
  } else {
    voltage <- audiomothField(raw, "battery(?: state)? was ([0-9]+\\.[0-9]{1,2})V")
  }

  temp <- audiomothField(raw, "temperature was ([-+]?[0-9]+\\.[0-9]+)C")

  external_mic <- grepl("using external microphone", raw)

  # Up to 1.5.0: "Low-pass filter applied with cut-off frequency of 5.0kHz.",
  # from 1.6.0: "Low-pass filter with frequency of 5.0kHz applied." Reading the
  # frequency out of the filter clause itself keeps it from picking up the
  # frequency trigger, which firmware from 1.7.0 writes earlier in the comment
  # and which also carries a value in kHz.
  filter <- FALSE
  filter_limit <- FALSE
  m <- audiomothMatch(
    raw,
    "(Low|High)-pass filter (?:applied )?with (?:cut-off )?frequency of ([0-9]+\\.[0-9]+)kHz"
  )
  if (!is.null(m)) {
    filter <- paste0(m[1], "-pass")
    filter_limit <- m[2]
  }
  m <- audiomothMatch(
    raw,
    paste0(
      "Band-pass filter (?:applied )?with (?:cut-off )?frequencies of ",
      "([0-9]+\\.[0-9]+)kHz and ([0-9]+\\.[0-9]+)kHz"
    )
  )
  if (!is.null(m)) {
    filter <- "Band-pass"
    filter_limit <- paste0(m[1], "-", m[2])
  }

  # Up to 1.5.0 the clause read "Recording cancelled before completion due to
  # <reason>", from 1.6.0 "Recording stopped due to <reason>" or, for the
  # magnetic switch, "Recording stopped by <reason>". The switch reason was
  # reworded in 1.6.0 and is reported under its original name.
  cancelled <- FALSE
  reason <- audiomothField(
    raw,
    "Recording (?:cancelled before completion|stopped) (?:due to|by) ([^.]+)\\."
  )
  if (!is.na(reason)) {
    if (reason == "switch position change") {
      reason <- "change of switch position"
    }
    cancelled <- reason
  }

  ret = list(
    "raw" = raw,
    "start_time" = start_time,
    "start_date" = start_date,
    "time_zone" = time_zone,
    "serial" = serial,
    "deployment_id" = deployment_id,
    "gain" = gain,
    "voltage" = voltage,
    "temperature" = temp,
    "external_mic" = external_mic,
    "filter" = filter,
    "filter.limit" = filter_limit,
    "cancelled" = cancelled
  )
  return(ret)
}
