#' Parse a filename
#'
#' Attempts to extract meaningful information from a filename, typically the
#' date and time a recording started.
#'
#' @details
#' ## Determining the format
#' It is sometimes impossible to accurately determine the format of
#' a filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers it
#' could be confused with a YYYYMMDD format. If a list of filenames is given
#' and the "match" format is specified then an effort will be made to determine
#' the most likely format that applies to all filenames.
#'
#' ## Timezones
#' Most supported formats write a wall clock (e.g. 20200609_191730) into the
#' filename, with no indication of the timezone the clock of the recorder was
#' set to. The `timezone` argument states which timezone that wall clock
#' belongs to, and defaults to UTC, which is what AudioMoth devices use unless
#' they are configured otherwise. The datetime returned is therefore the same
#' instant whatever timezone the machine running the analysis is set to.
#'
#' The 'AudioMoth HEX' and 'timestamp' formats are exceptions: these encode an
#' instant rather than a wall clock, so for them `timezone` only sets the
#' timezone the datetime is displayed in.
#'
#' ## Supported formats
#' * **AudioMoth** - The newer format for AudioMoth devices consists of a
#'   standard YYYYMMDD_HHMMSS.wav format. An eight character hexadecimal
#'   filename given this format is treated as 'AudioMoth HEX'.
#' * **AudioMoth HEX** - Older format for AudioMoth devices consisting of eight
#'   hexadecimal characters giving the number of seconds since
#'   1970-01-01 00:00:00 UTC. Conversion is handled by a call to the
#'   `audiomoth()` function from the `seewave` package
#'   \insertCite{seewave2008}{sonicscrewdriver}.
#' * **timestamp** - A standard date-time format. Uses the R standard origin of
#'   1970-01-01 00:00:00 UTC.
#' * **Wildlife Acoustics SM2** - Can also be used for Wildlife Acoustics SM4
#'   devices. The filename is split by a call to `seewave::songmeter()`.
#' * **Wildlife Acoustics SM3** - The filename is split by a call to
#'   `seewave::songmeter()`.
#' * **YYYYMMDD_HHMMSS** - A standard date-time format.
#' * **YYYYMMDD** - A standard date format, the time is taken to be midnight.
#'
#' @param file A filename (or list of filenames).
#' @param format Optionally force a given format (see Details). If NULL (default)
#'  an attempt is made to automatically detect the format for each file. If "match"
#'  and a list of filenames is given then an attempt will be made to find a format
#'  that matches all files. This may give incorrect results if the filename is
#'  ambiguous (see Details).
#' @param timezone The timezone the clock of the recorder was set to, defaults
#'  to "UTC" (see Details).
#' @return A list of file, type of match, datetime. If a list of filenames is
#'  given then a list of such lists is returned.
#' \cr\cr
#' It is possible to determine additional properties from some files, these will
#' be added to the list.
#' @references
#'   \insertAllCited{}
#' @export
#' @examples
#' parseFilename("5E90A4D4.wav")
#'
parseFilename <- function(file, format=NULL, timezone="UTC") {
  if (is.null(timezone)) {
    timezone <- "UTC"
  }
  if (!is.null(format) && identical(as.character(format), "match")) {
    # Matching a format across files is only meaningful for a list of files,
    # for a single file it is the same as detecting the format.
    format <- if (is(file, "list")) .matchFormat(file) else NULL
  }
  if (is(file, "list")) {
    return(lapply(file, parseFilename, format=format, timezone=timezone))
  }
  if (is.null(format)) {
    format <- .detectFormat(file)
    if (is.null(format)) {
      stop("Could not determine format of ", file)
    }
  }
  .validateChoice(format, .knownFileFormats(), msg=paste("Unknown format:", format))

  bn <- tools::file_path_sans_ext(basename(file))

  if (format %in% c("AudioMoth HEX", "AudioMoth")) {
    if (grepl("^[0-9]{8}_[0-9]{6}$", bn)) {
      # Newer AudioMoth filenames hold a wall clock rather than an instant, and
      # seewave::audiomoth() parses these in the session timezone whatever it is
      # passed, so they are parsed here instead.
      datetime <- .parseWallClock(bn, "%Y%m%d_%H%M%S", timezone)
    } else {
      data <- seewave::audiomoth(basename(file), tz=timezone)
      datetime <- data[,"time"]
      attr(datetime, "tzone") <- timezone
    }
    return(list(
      filename = file,
      match=format,
      datetime = datetime
    ))
  }
  if (format %in% c("Wildlife Acoustics SM2", "Wildlife Acoustics SM3")) {
    data <- seewave::songmeter(basename(file))
    # seewave::songmeter() parses the wall clock in the session timezone, so the
    # instant is rebuilt here from the parts of the filename it returns.
    ret <- (list(
      filename = file,
      match=format,
      datetime = ISOdatetime(
        data[,"year"], data[,"month"], data[,"day"],
        data[,"hour"], data[,"min"], data[,"sec"],
        tz=timezone
      ),
      model = data[,"model"],
      prefix = data[,"prefix"],
      mic = data[,"mic"],
      geo = data[,"geo"]
    ))
    return(ret)
  }
  if (format == "YYYYMMDD") {
    datetime <- .parseWallClock(bn, "%Y%m%d", timezone)
  }
  if (format == "YYYYMMDD_HHMMSS") {
    datetime <- .parseWallClock(bn, "%Y%m%d_%H%M%S", timezone)
  }
  if (format == "timestamp") {
    #A timestamp counts from the epoch in UTC wherever it is read. Taking the
    #origin in whatever zone the session happened to be in moved the instant by
    #that zone's offset, and the timezone argument was not used at all.
    datetime <- as.POSIXct(
      as.numeric(bn),
      origin = as.POSIXct("1970-01-01", tz="UTC"),
      tz = timezone
    )
  }
  return(list(
    filename = file,
    match=format,
    datetime = datetime
  ))
}

.parseWallClock <- function(bn, format, timezone) {
  return(as.POSIXct(strptime(bn, format, tz=timezone), tz=timezone))
}

.matchFormat <- function(file) {
  for (alternative in c(0, 1)) {
    formats <- lapply(file, .detectFormat, alternative=alternative)
    if (any(vapply(formats, is.null, logical(1)))) {
      next
    }
    same <- vapply(
      formats,
      function(f) identical(as.character(f), as.character(formats[[1]])),
      logical(1)
    )
    if (all(same)) {
      return(formats[[1]])
    }
  }
  stop("Could not find a single format matching all filenames")
}

.knownFileFormats <- function() {
  return(c(
    "AudioMoth HEX",
    "AudioMoth",
    "timestamp",
    "Wildlife Acoustics SM2",
    "Wildlife Acoustics SM3",
    "YYYYMMDD",
    "YYYYMMDD_HHMMSS"
  ))
}

.detectFormat <- function(file, alternative=0) {
  bn <- tools::file_path_sans_ext(basename(file))
  # Check for timestamp
  if (grepl("^[0-9]{10}$", bn)) {
    return("timestamp")
  }
  # Check for YYYMMDD
  if (grepl("^[0-9]{8}$", bn)) {
    if (alternative == 1) {
      return("AudioMoth HEX")
    }
    return("YYYYMMDD")
  }
  # Check for AudioMoth old hexadecimal
  if (grepl("^[0-9A-Fa-f]{8}$", bn)) {
    format <- "AudioMoth HEX"
    if (file_ext(file) != "wav") {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }

  # Check for YYYYMMDD_HHMMSS
  if (grepl("^[0-9]{8}_[0-9]{6}$", bn)) {
    return("YYYYMMDD_HHMMSS")
  }

  # Check for Wildlife Acoustics format PREFIX_YYYYMMDD_HHMMSS
  if (grepl("^[^_]*_[0-9]{8}_[0-9]{6}$", bn)) {
    format <- "Wildlife Acoustics SM2"
    if (!file_ext(file) %in% c("wav", "wac")) {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }

  # Check for Wildlife Acoustics SM3 format
  if (grepl("^[^_]*_[01+-]{3}_[0-9]{8}[_$][0-9]{6}$", bn)) {
    format <- "Wildlife Acoustics SM3"
    if (!file_ext(file) %in% c("wav", "wac")) {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }
  return(NULL)
}
