#' Parse a filename
#'
#' Attempts to extract meaningful information from a filename.
#'
#' @param string A filename
#' @param format Optionally force a given format - "timestamp"
#' @return A list of raw results, plus calculated values for date, time and device.
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' parseFilename("20180605.wav")
#'
parseFilename <- function(string, format=NULL) {
  date_calculated <- list()
  time_calculated <- list()
  device_calculated <- list()

  if (!is.null(format)) {
    if (format=="timestamp") {
      string <- file_path_sans_ext(string)
      date_calculated <- c(as.Date(as.POSIXct(string, origin="1970-01-01", format="%s")))
      time_calculated <- c(format(as.POSIXlt(string, origin="1970-01-01", format="%s"), format="%H%M"))
      return(list(
        date = list(
          "Calculated" = date_calculated
        ),
        time = list(
          "Calculated" = time_calculated
        ),
        device = list(
        )
      ))
    }
  }

  yyyymmdd <- gregexpr("((19|20)\\d{2})(-|_| - )?(0[1-9]|1[0-2])(-|_| - )?((0|1|2)[1-9]|(3[0|1]))",string)
  if (yyyymmdd[[1]][[1]] != -1) {
    date_calculated <- c(date_calculated, lapply(yyyymmdd[[1]], parseStringYYYYMMDD, string=string))
  }

  hhmmss <- gregexpr("(^|\\D)((0|1)[0-9]|2[0-3])(:| : )?([0-5][0-9])(:| : )?([0-5][0-9])(\\D|$)", string)
  if (hhmmss[[1]][[1]] != -1) {
    time_calculated <- c(time_calculated, lapply(hhmmss[[1]], parseStringHHMMSS, string=string))
  }

  songmeter <- gregexpr("SM[1-4]", string)
  if (songmeter[[1]][[1]] != -1) {
    device_calculated <- c(device_calculated, lapply(songmeter[[1]], parseStringSongMeter, string=string))
  }

  return(list(
    date = list(
      "YYYYMMDD" = yyyymmdd,
      "Calculated" = date_calculated
    ),
    time = list(
      "HHMMSS" = hhmmss,
      "Calculated" = time_calculated
    ),
    device = list(
      "songmeter" = songmeter,
      "Calculated" = device_calculated
    )
  ))
}

parseStringYYYYMMDD <- function(start, string) {
  string <- substring(string, start, start+13)
  string <- gsub(" ", "", string, fixed = TRUE)
  string <- gsub("-", "", string, fixed = TRUE)
  string <- gsub("_", "", string, fixed = TRUE)
  print(string)
  return(list(
    year  = substring(string,1,4),
    month = substring(string,5,6),
    day   = substring(string,7,8)
  ))
}

parseStringHHMMSS <- function(start, string) {
  string <- substring(string, start, start+11)
  string <- gsub(" ", "", string, fixed = TRUE)
  string <- gsub(":", "", string, fixed = TRUE)
  return(list(
    hour   = substring(string,1,2),
    minute = substring(string,3,4),
    second = substring(string,5,6)
  ))
}

parseStringSongMeter <- function(start, string) {
  return(list(
    device = substring(string, start, start+2)
  ))
}
