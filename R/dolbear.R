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
  #Neither argument was checked at all, so a negative rate of chirping gave a
  #temperature as though it were a reading.
  if (!is.null(n)) {
    .validateNumericRange(
      n, "Chirps per minute", min=0,
      range.msg="Chirps per minute must not be negative.",
      type.msg="Chirps per minute must be numeric."
    )
  }
  if (!is.null(t)) {
    .validateNumericRange(
      t, "Temperature", min=-273.15,
      range.msg="Temperature must be above absolute zero.",
      type.msg="Temperature must be numeric."
    )
  }

  data <- dolbear.data()

  if (!is.null(species)) {
    if (!species %in% data$species) {
      stop(paste("dolbear:", species, "is not a known species."))
    }
    data <- data[which(data$species==species),]
  }
  #Each value asked about is calculated for every population in the table. Left
  #as they were, a vector of values was recycled against the rows, so each value
  #was paired with a different regression line rather than tried against them all.
  values <- if (is.null(t)) n else t
  rows <- rep(seq_len(nrow(data)), times=length(values))
  values <- rep(values, each=nrow(data))
  data <- data[rows, , drop=FALSE]
  rownames(data) <- NULL

  if (is.null(t)) {
    n <- values
    t <- (n - data$c)/data$m
  } else {
    t <- values
    n <- data$m*t + data$c
    #Every regression in the table crosses zero between about 3.9 and 4.7
    #degrees, so below that each returns a rate of chirping that is negative:
    #dolbear(t=3) gave -6.85, -8.93 and -13.99 chirps per minute without a word,
    #and dolbear(t=-40) gave -341.7.
    negative <- !is.na(n) & n < 0
    if (any(negative)) {
      stop(paste0(
        "Dolbear's law gives a negative rate of chirping at ",
        paste(unique(t[negative]), collapse=", "),
        " degrees Celsius, below every temperature its regressions were ",
        "fitted over."
      ))
    }
  }

  #The table carries the range of temperature each regression was fitted over,
  #and nothing read it. The answer outside that range is still given, since it
  #is a line that can be extended, but the extrapolation is no longer silent.
  outside <- !is.na(t) & (t < data$min | t > data$max)
  if (any(outside)) {
    warning(paste0(
      "Dolbear's law is extrapolated beyond the temperatures it was fitted ",
      "over for ",
      paste(
        unique(paste0(
          data$species[outside], ", ", data$location[outside],
          " (", data$min[outside], "-", data$max[outside], " C)"
        )),
        collapse="; "
      ),
      "."
    ))
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
