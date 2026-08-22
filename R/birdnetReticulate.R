#' Analyse sound files using BirdNET-Analyzer
#'
#' This function takes a list of sound files and analyses them using the
#' `BirdNET-Analyzer` \insertCite{birdnet2021}{sonicscrewdriver}. The function
#' either returns a data frame with the results of the analysis or a list of
#' `Annotation` objects.
#'
#' @param files A character vector of file paths.
#' @param lat A latitude or  vector of latitudes.
#' @param lon A longitude or  vector of longitudes.
#' @param date A `Date` or list of `Date` objects .
#' @param output One of "data.frame" or "Annotation".
#' @references
#'  \insertAllCited{}
#' @return A list of Annotation objects, or a data frame if output is "data.frame".
#' @export
#' @examples
#' \dontrun{
#'   birdnetAnalyse(files=c("path/to/file1.wav", "path/to/file2.wav"), output="data.frame")
#' }
birdNetAnalyse <- function(files, lat=NULL, lon=NULL, date=NULL, output="Annotation") {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to use BirdNET.")
  }
  .useUtf8Locale()
  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    pythonInstall()
  }

  reticulate::use_virtualenv("sonicscrewdriver")

  .validateChoice(output, c("data.frame", "Annotation"), msg="Unknown output format.")
  if (!is.null(lat)) {
    if (is.null(lon)) {
      stop("If lat is provided, lon must also be provided.")
    }
    if (length(lat) == 1) {
      lat <- rep(lat, length(files))
    }
    if (length(lon) == 1) {
      lon <- rep(lon, length(files))
    }
  }
  if (!is.null(date)) {
    if (length(date) == 1) {
      date <- rep(date, length(files))
    }
  }

  reticulate::use_virtualenv("sonicscrewdriver")
  bn <- reticulate::import("birdnetlib")
  bna <- reticulate::import("birdnetlib.analyzer")
  datetime <- reticulate::import("datetime")
  analyzer <- bna$Analyzer()

  #A list of dates formatted one element at a time gave a one element list to
  #format(), and as.integer() then read NA for the year, month and day.
  if (!is.null(date)) {
    date <- as.Date(do.call(c, as.list(date)))
  }

  ret <- vector("list", length(files))
  for (i in seq_along(files)) {
    if (!is.null(date)) {
      d <- datetime$date(
        year = as.integer(format(date[i], "%Y")),
        month = as.integer(format(date[i], "%m")),
        day = as.integer(format(date[i], "%d"))
      )
    } else {
      d <- NULL
    }
    recording <- bn$Recording(
      analyzer,
      files[i],
      lat = lat[i],
      lon = lon[i],
      date = d
      )
    recording$analyze()

    #Collected per file and joined once at the end. Growing the list a detection
    #at a time copied the whole of it on every one.
    ret[[i]] <- lapply(recording$detections, function(detection) {
      annotation(
        file = files[i],
        start = detection$start_time,
        end = detection$end_time,
        source = "BirdNet-Analyzer",
        type = "birdnet-detection",
        value = detection$label,
        metadata = list(
          "confidence" = detection$confidence,
          "common_name" = detection$common_name,
          "scientific_name" = detection$scientific_name
        )
      )
    })
  }
  ret <- do.call(c, ret)
  if (output=="Annotation") {
    return(ret)
  }
  return(AnnotationList2DataFrame(ret))
}
