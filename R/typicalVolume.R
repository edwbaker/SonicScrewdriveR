#' Typical volumes
#'
#' Typical volumes of everyday things.
#'
#' @param thing Volume of thing, if missing then returns all volumes
#' @export
#' @return Typical volume of thing in dBA, or if no thing parameter a data frame of all volumes
#' @examples
#' typicalVolume()
#' typicalVolume("rocket")
#'
typicalVolume <- function(thing="") {
  tv <- typicalVolumes()
  if (thing %in% row.names(tv)) {
    return(t[match(thing,row.names(t)),][[1]])
  }
  return(tv)
}

typicalVolumes <- function() {
  # Source: Murray Schaefer Soundscapes
  ret <- list(
    "steam engine" = 85,
    "printing works" = 87,
    "diesel electric generator" = 96,
    "screw-heading machine" = 101,
    "weaving shed" = 104,
    "sawmill chipper" = 105,
    "metalwork grinder" = 106,
    "wood-planing machine" = 108,
    "metal saw" = 110,
    "rock band" = 115,
    "boiler works" = 118,
    "metal hammering" = 118,
    "jet take-off" = 120,
    "rocket launch" = 160
  )
  return(t(as.data.frame(t(ret))))
}
