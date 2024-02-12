#' Convert text times of day in audioblast traits to numeric values
#'
#' This function takes a traits dataset retrieved from audioblast and converts
#' values such as "dawn" into a numeric time of day based on the date and location.
#' @param traits Traits dataset retrieved using audioblast()
#' @param date The date used for conversion for time
#' @param lat Latitude of location
#' @param lon Longitude of location
#' @export
ab_diel_traits <- function(traits, date, lat, lon) {
  cn <- colnames(traits)
  if (!"value_min" %in% cn) {
    value_min <- vector(mode="character", length=nrow(traits))
    traits <- cbind(traits, value_min)
  }
  if (!"value_max" %in% cn) {
    value_max <- vector(mode="character", length=nrow(traits))
    traits <- cbind(traits, value_max)
  }

  update <- .calcTimesOfDay(traits$value, traits$value_min, traits$value_max, date,lat,lon)
  traits$value_min <- update$min
  traits$value_max <- update$max

  traits$value_min <- as.numeric(traits$value_min)
  traits$value_max <- as.numeric(traits$value_max)

  return(traits)
}

#' @importFrom stringi stri_replace_all_charclass stri_pad
.calcTimesOfDay <- function(times, min, max, date, lat, lon) {
  #Some initial tidying
  times <- tolower(stri_replace_all_charclass(times, "\\p{WHITE_SPACE}", ""))
  min[is.na(min)] <- ""
  max[is.na(max)] <- ""

  #Load time data
  tod <- .timesOfDay(date, lat, lon)

  for (i in 1:length(times)) {
    #If min and max already set then skip
    if (min[i] != "" & max[i] != "") {next}
    split <- strsplit(times[i], split="-")[[1]]
    if (length(split) > 2) {warning("Cannot split on more than one '-'")}
    if (length(split) == 1) {
      inf <- tod[tod$times==times[i],]
      if (nrow(inf) == 1) {
        min[i] <- inf$starts
        max[i] <- inf$ends
      } else {
        print(times[i])
      }
    }
    if (length(split) == 2) {
      inf1 <- tod[tod$times==split[1],]
      inf2 <- tod[tod$times==split[2],]
      if (nrow(inf1) == 1 & nrow(inf2) == 1) {
        min[i] <- inf1$starts
        max[i] <- inf2$ends
      } else {
        print(times[i])
      }
    }
  }
  return(as.data.frame(cbind(min,max)))
}

.timesOfDay <- function(date=NULL, lat=NULL, lon=NULL) {
  times <- c(
    "day",
    "night",
    "dayandnight",
    "afternoon",
    "evening"
  )
  if (is.null(date)) {return(times)}

  d <- getSunlightTimes(date=date, lat=lat, lon=lon)
  starts <- c(
    paste0(
      stri_pad(as.POSIXlt(d$sunrise)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$sunrise)$min, 2, "left", 0)
    ),
    paste0(
      stri_pad(as.POSIXlt(d$night)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$night)$min, 2, "left", 0)
    ),
    "0000",
    paste0(
      stri_pad(as.POSIXlt(d$solarNoon)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$solarNoon)$min, 2, "left", 0)
    ),
    paste0(
      stri_pad(as.POSIXlt(d$sunset)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$sunset)$min, 2, "left", 0)
    )
  )
  ends <- c(
    paste0(
      stri_pad(as.POSIXlt(d$sunset)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$sunset)$min, 2, "left", 0)
    ),
    paste0(
      stri_pad(as.POSIXlt(d$nightEnd)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$nightEnd)$min, 2, "left", 0)
    ),
    "2359",
    paste0(
      stri_pad(as.POSIXlt(d$sunset)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$sunset)$min, 2, "left", 0)
    ),
    paste0(
      stri_pad(as.POSIXlt(d$night)$hour, 2, "left", 0),
      stri_pad(as.POSIXlt(d$night)$min, 2, "left", 0)
    )
  )
  ret <- as.data.frame(cbind(times,starts,ends))
  return(ret)
}

