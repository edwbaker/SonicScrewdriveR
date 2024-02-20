#' Parse a filename
#'
#' Attempts to extract meaningful information from a filename.
#' @details
#' ## Determining the format
#' It is sometimes impossible to accurately determine the format of
#' a filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers it
#' could be confused with a YYYYMMDD format. If a list of filenames is given
#' and no format is specified then an effort will be made to determine the most
#' likely format that applies to all filenames.
#'
#' ## Supported formats
#' * **AudioMoth** - The newer format for AudioMoth devices consists of a
#'   standard YYYYMMDD_HHMMSS.wav format. Specifying 'AudioMoth' forces a call
#'   to the `audiomoth()` function from the `seewave` package
#'   \insertCite{seewave2008}{sonicscrewdriver}.
#' * **AudioMoth HEX** - Older format for AudioMoth devices consisting of eight
#'   hexadecimal characters. Conversion is handled by a call to
#'   `seewave::audiomoth()`.
#'
#' @param file A filename (or list of filenames).
#' @param format Optionally force a given format (see Details). If NULL (default)
#'  an attempt is made to automatically detect the format. This may give incorrect
#'  results if the filename is ambiguous (see Details)
#' @param timezone Optionally set a timezone.
#' @return A list of file, type of match, datetime.
#' @references
#'   \insertAllCited{}
#' @export
#' @examples
#' parseFilename("5E90A4D4.wav")
#'
parseFilename <- function(file, format=NULL, timezone=NULL) {
  if (is(file, "list")) {
    return(lapply(file, parseFilename, format=format, timezone=timezone))
  }
  if (is.null(format)) {
    format <- .detectFormat(file)
  }
  if (format %in% c("AudioMoth HEX", "AudioMoth")) {
    if (is.null(timezone)) {
      tz <- ""
    } else {
      tz <- timezone
    }
    data <- seewave::audiomoth(file, tz=tz)
    return(list(
      filename = file,
      match=format,
      datetime = data[,"time"]
    ))
  }
}

.detectFormat <- function(file) {
  file <- tools::file_path_sans_ext(basename(file))
  # Check for AudioMoth old hexadecimal
  if (grepl("^[0-9A-Fa-f]{8}$", file)) {
    return("AudioMoth HEX")
  }
  return(NULL)
}
