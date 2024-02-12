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
  if (is.na(thing)) {
    return(tv)
  }
  if (thing %in% tv[,1]) {
    return(as.numeric(tv[tv$thing==thing,2]))
  } else {
    stop("Thing not found.")
  }
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
  ret <- as.data.frame(cbind(n, as.numeric(vals)))
  colnames(ret) <- c("thing", "dBA")

  return(ret)
}
