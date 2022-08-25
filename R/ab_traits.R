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

  update <- calcTimesOfDay(traits$value, traits$value_min, traits$value_max, date,lat,lon)
  traits$value_min <- update$min
  traits$value_max <- update$max

  traits$value_min <- as.numeric(traits$value_min)
  traits$value_max <- as.numeric(traits$value_max)

  return(traits)
}
