#' Dolbear's law
#'
#' \insertCite{dolbear1897;textual}{sonicscrewdriver} was the first to publish a formula for
#' how the rate of chirping of crickets varies with temperature, using the tree cricket
#' \emph{Oecanthus fultoni}.
#'
#' Subsequent research by Dolbear and others have published additional formulae for other
#' species: \emph{Oecanthus fultoni} \insertCite{fulton1925}{sonicscrewdriver},
#' \emph{Oecanthus rileyi} \insertCite{walker1962}{sonicscrewdriver},
#' \emph{Oecanthus alexanderi} \insertCite{walker2010}{sonicscrewdriver}, and
#' \emph{Oecanthus allardi} \insertCite{allard1957}{sonicscrewdriver}.
#'
#' @param n Chirps per minute
#' @param t Temperature in Celsius
#' @param species Species to use (by default \emph{Oecanthus fultoni}), NULL to
#'   calculate for all species.
#' @references
#'   \insertAllCited{}
#' @return Data frame with t and n calculated for matching species.
#' @export
#' @examples
#' dolbear(n=6)
#' dolbear(t=25)
#'
dolbear <- function(n=NULL, t=NULL, species="Oecanthus fultoni") {
  if (is.null(n) & is.null(t)) {
    stop("Dolbear's law calculation requires either n or t to be specified.")
  }

  data <- dolbear.data()

  if (!is.null(species)) {
    if (!species %in% data$species) {
      stop(paste("dolbear:", species, "is not a known species."))
    }
    data <- data[which(data$species==species),]
  }
  m <- data$m
  c <- data$c

  if (is.null(t)) {
    t <- (n-c)/m
  }
  if (is.null(n)) {
    n <- m*t+c
  }

  return(cbind(data,t,n))
}

dolbear.data <- function() {
  data.frame(
    species=c(
      "Oecanthus fultoni",
      "Oecanthus fultoni",
      "Oecanthus fultoni",
      "Oecanthus rileyi",
      "Oecanthus alexanderi",
      "Oecanthus allardi"
    ),
    location=c(
      "Iowa",
      "Oregon",
      "Ohio",
      "Oregon",
      "Texas",
      "Dominican Republic"
    ),
    m=c(
      7.7879,
      9.2007,
      8.208,
      5.2025,
      1.4963,
      0.8693
    ),
    c=c(
      -30.21,
      -36.53,
      -38.61,
      -18.08,
      -0.52,
      -0.58
    ),
    min=c(
      15,
      9,
      18,
      8,
      23,
      19
    ),
    max=c(
      25,
      28,
      31,
      29,
      28,
      25
    ),
    source=c(
      "Fulton, 1925",
      "Fulton, 1925",
      "Walker, 1962",
      "Fulton, 1925",
      "Walker & Collins, 2010",
      "Allard, 1957"
    )
  )
}
