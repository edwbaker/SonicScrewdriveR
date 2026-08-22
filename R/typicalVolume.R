#' Typical volumes
#'
#' Typical volumes of everyday things.
#'
#' @param thing Volume of thing, if missing then returns all volumes
#' @export
#' @return Typical volume of thing in dBA, or if no thing parameter a data frame of all volumes
#' @examples
#' typicalVolume()
#' typicalVolume("rocket launch")
#'
typicalVolume <- function(thing=NA_character_) {
  tv <- .typicalVolumes()
  #Length safe, as is.na() on a vector gave a condition of length greater than
  #one and on NULL a condition of length zero.
  if (length(thing) == 0 || all(is.na(thing))) {
    return(tv)
  }
  missing <- setdiff(thing, tv$thing)
  if (length(missing) > 0) {
    stop(paste("Thing not found:", paste(missing, collapse=", ")))
  }
  return(tv$dBA[match(thing, tv$thing)])
}

.typicalVolumes <- function() {
  # Source: Murray Schafer Soundscapes
  n <- c(
    "steam engine",
    "printing works",
    "diesel electric generator",
    "screw-heading machine",
    "weaving shed",
    "sawmill chipper",
    "metalwork grinder",
    "wood-planing machine",
    "metal saw",
    "rock band",
    "boiler works",
    "metal hammering",
    "jet take-off",
    "rocket launch"
  )
  vals <- c(
    85,
    87,
    96,
    101,
    104,
    105,
    106,
    108,
    110,
    115,
    118,
    118,
    120,
    160
  )
  #Built directly, as cbind() of the names and the values gives a character
  #matrix and so a data frame whose dBA column holds strings.
  return(data.frame(thing = n, dBA = vals, stringsAsFactors = FALSE))
}
