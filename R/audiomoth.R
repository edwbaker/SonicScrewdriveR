#' Read AudioMoth configuration file
#'
#' Reads and parses an AudioMoth configuration file.
#'
#' @param filename Path to the configuration file to read
#' @return A data frame of matching annotations
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
  c[,1] <- trimws(c[,1])
  colnames(c) <- c("Key", "Value")
  return(c)
}

#' Read AudioMoth metadata from a wave file
#'
#' Reads and parses metadata stored in wave files produced by
#' AudioMoth devices.
#'
#' @param filename Path to the wave file to read
#' @return A list of extracted parameters
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

  r <- regexpr("[0-2][0-9]:[0-6][0-9]:[0-6][0-9]", raw)
  start_time <- substr(raw, r, r+attr(r, "match.length")-1)

  r <- regexpr("[0-3][0-9]/[0-1][0-9]/[0-9]{4}", raw)
  start_date <- substr(raw, r, r+attr(r, "match.length")-1)

  r <- regexpr("\\(", raw)
  l <- regexpr("\\)", raw)
  time_zone <- substr(raw,r+1,l-1)

  r <- regexpr("AudioMoth", raw) + 10
  serial <- substr(raw, r, r+16)

  r <- regexpr("AudioMoth [0-9|A-Z]{16} at ", raw) + 30
  l <- regexpr("gain setting", raw) - r -2
  gain <- substr(raw, r, r+l)

  if ( regexpr("less than 2\\.5V", raw) != -1) {
    voltage <- "<2.5"
  } else if (regexpr("greater than 4\\.9V", raw) != -1) {
    voltage <- ">4.9"
  } else {
    r <- regexpr("[0-9].[0-9]V", raw)
    voltage <- substr(raw, r, r+2)
  }

  r <- regexpr("[-+]?[0-9]+[\\.]+[0-9]+[C]", raw)
  if (r != -1) {
    temp <- substr(raw, r, r+attr(r, "match.length")-2)
  } else {
    temp <- NA
  }

  r <- regexpr("using external microphone", raw)
  external_mic <- if (r == -1) FALSE else TRUE

  filter <- FALSE
  filter_limit <- FALSE
    filter.limit <-
  if (regexpr("Low-pass filter applied", raw) != -1) {
    filter <- "Low-pass"
  }
  if (regexpr("Band-pass filter applied", raw) != -1) {
    filter <- "Band-pass"
  }
  if (regexpr("High-pass filter applied", raw) != -1) {
    filter <- "High-pass"
  }
  if (is.element(filter, c("Low-pass", "High-pass"))) {
    r <- regexpr("frequency of", raw) + 13
    l <- regexpr("kHz", raw) - 1
    filter_limit <- substr(raw,r,l)
  }
  if (filter == "Band-pass") {
    al <- regexpr("frequencies of", raw) + 15
    ar <- regexpr("kHz",  raw) - 1
    bl <- regexpr("kHz and", raw) + 8
    br <- regexpr("kHz\\.", raw) - 1
    filter_limit(paste0(substr(raw,al, ar),"-",substr(raw,bl,br)))
  }

  cancelled <- FALSE
  if (regexpr("microphone change.", raw) != -1) {
    cancelled <- "microphone change"
  }
  if (regexpr("change of switch position.", raw) != -1) {
    cancelled <- "change of switch position"
  }
  if (regexpr("low voltage.", raw) != -1) {
    cancelled <- "low voltage"
  }
  if (regexpr("file size limit.", raw) != -1) {
    cancelled <- "file size limit"
  }

  ret = list(
    "raw" = raw,
    "start_time" = start_time,
    "start_date" = start_date,
    "time_zone" = time_zone,
    "serial" = serial,
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
