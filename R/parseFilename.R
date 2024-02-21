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
#' ## Supported formats
#' * **AudioMoth** - The newer format for AudioMoth devices consists of a
#'   standard YYYYMMDD_HHMMSS.wav format. Specifying 'AudioMoth' forces a call
#'   to the `audiomoth()` function from the `seewave` package
#'   \insertCite{seewave2008}{sonicscrewdriver}.
#' * **AudioMoth HEX** - Older format for AudioMoth devices consisting of eight
#'   hexadecimal characters. Conversion is handled by a call to
#'   `seewave::audiomoth()`.
#' * **timestamp** - A standard date-time format. Uses the R standard origin of
#'   1970-01-01 00:00:00 UTC.
#' * **Wildlife Acoustics SM2** - Can also be used for Wildlife Acoustics SM4
#'   devices. Conversion is handled by a call to `seewave::songmeter()`.
#' * **Wildlife Acoustics SM3** - Conversion is handled by a call to
#'   `seewave::songmeter()`.
#' * **YYYYMMDD_HHMMSS** - A standard date-time format.
#'
#' @param file A filename (or list of filenames).
#' @param format Optionally force a given format (see Details). If NULL (default)
#'  an attempt is made to automatically detect the format for each file. If "match"
#'  and a list of filenames is given then an attempt will be made to find a format
#'  that matches all files. This may give incorrect results if the filename is
#'  ambiguous (see Details).
#' @param timezone Optionally set a timezone.
#' @return A list of file, type of match, datetime.
#' \cr\cr
#' It is possible to determine additional properties from some files, these will
#' be added to the list.
#' @references
#'   \insertAllCited{}
#' @export
#' @examples
#' parseFilename("5E90A4D4.wav")
#'
parseFilename <- function(file, format=NULL, timezone=NULL) {
  if (is(file, "list")) {
    if (!is.null(format)) {
      if (format == "match") {
        formats <- lapply(file, .detectFormat)
        if (all(formats == formats[[1]])) {
          format <- formats[[1]]
          return(lapply(file, parseFilename, format=format, timezone=timezone))
        } else {
          formats <- lapply(file, .detectFormat, alternative=1)
          if (all(formats == formats[[1]])) {
            format <- formats[[1]]
            return(lapply(file, parseFilename, format=format, timezone=timezone))
          }
        }
      }
    } else {
      return(lapply(file, parseFilename, format=format, timezone=timezone))
    }
  }
  if (is.null(format)) {
    format <- .detectFormat(file)
    if (is.null(format)) {
      stop("Could not determine format of ", file)
    }
  }
  if (!format %in% .knownFileFormats()) {
    stop(paste("Unknown format:", format))
  }
  if (format %in% c("AudioMoth HEX", "AudioMoth")) {
    if (is.null(timezone)) {
      tz <- ""
    } else {
      tz <- timezone
    }
    data <- seewave::audiomoth(file, tz=tz)
    if (attr(data[1,"time"], "tzone") == "") {
      attr(data[,"time"], "tzone") <- tz
    }
    return(list(
      filename = file,
      match=format,
      datetime = data[,"time"]
    ))
  }
  if (format %in% c("Wildlife Acoustics SM2", "Wildlife Acoustics SM3")) {
    data <- seewave::songmeter(file)
    ret <- (list(
      filename = file,
      match=format,
      datetime = as.POSIXct(data[,"time"], tz=timezone),
      model = data[,"model"],
      prefix = data[,"prefix"],
      mic = data[,"mic"],
      geo = data[,"geo"]
    ))
    return(ret)
  }
  if (format == "YYYYMMDD") {
    if (is.null(timezone)) {
      timezone <- "UTC"
    }
    datetime <- as.POSIXct(strptime(tools::file_path_sans_ext(basename(file)), "%Y%m%d"), tz=timezone)
  }
  if (format == "YYYYMMDD_HHMMSS") {
    if (is.null(timezone)) {
      timezone <- "UTC"
    }
    datetime <- as.POSIXct(strptime(tools::file_path_sans_ext(basename(file)), "%Y%m%d_%H%M%S"), tz=timezone)
  }
  if (format == "timestamp") {
    datetime <- as.POSIXct(as.numeric(tools::file_path_sans_ext(basename(file))), origin=as.POSIXct("1970-01-01"))
  }
  return(list(
    filename = file,
    match=format,
    datetime = datetime
  ))
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
