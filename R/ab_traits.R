ab_diel_traits <- function(traits, date, lat, lon) {
  update <- calcTimesOfDay(traits$value, traits$value_min, traits$value_max, date,lat,lon)
  traits$value_min <- update$min
  traits$value_max <- update$max

  return(traits)
}
