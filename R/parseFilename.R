#' Parse a filename
#'
#' Attempts to extract meaningful information from a filename.
#'
#' @param string A filename.
#' @param format Optionally force a given format - "timestamp".
#' @param timezone Optionally set a timezone.
#' @return A list of string, type of match, datetime.
#' @export
#' @examples
#' parseFilename("5E90A4D4.wav")
#'
parseFilename <- function(string, format=NULL, timezone=NULL) {
  if (is(string, "list")) {
    return(lapply(string, parseFilename, format=format, timezone=timezone))
  }
  if (is.null(format)) {
    format <- .detectFormat(string)
  }
  if (format %in% c("AudioMoth HEX")) {
    if (is.null(timezone)) {
      tz <- ""
    } else {
      tz <- timezone
    }
    data <- seewave::audiomoth(string, tz=tz)
    return(list(
      filename = string,
      match=format,
      datetime = data[,"time"]
    ))
  }
}

.detectFormat <- function(string) {
  string <- tools::file_path_sans_ext(basename(string))
  # Check for AudioMoth old hexadecimal
  if (grepl("^[0-9A-Fa-f]{8}$", string)) {
    return("AudioMoth HEX")
  }
  return(NULL)
}
