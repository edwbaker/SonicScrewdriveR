#' Calculate and plot statistics on a  frequency spectrum
#'
#' Given a list of outputs from meanspec generates a plot with the mean shown by
#' a line, and either the minimum/maximum values or one standard deviation
#' shown by a ribbon.
#'
#' @param spectra A list of spectra
#' @param stats Either minMax or sd
#' @param line.col Colour for the line
#' @param ribbon.col Colour for the ribbon
#' @return A ggplot2 object
#' @importFrom ggplot2 aes element_blank geom_line geom_ribbon labs theme .data
#' @export
#'
specStats <- function(spectra, stats="minMax", line.col="black", ribbon.col="grey70") {
  .validateChoice(stats, c("minMax", "sd"), "stats", "specStats", prep="for")
  if (!is.list(spectra) || length(spectra) == 0) {
    stop("specStats requires a list of one or more spectra.")
  }
  if (stats == "sd" && length(spectra) < 2) {
    stop("Two or more spectra are required for a standard deviation.")
  }

  validateSpectrum(spectra[[1]])
  for (i in seq_along(spectra)[-1]) {
    validateComparableSpectra(spectra[[1]], spectra[[i]])
  }

  if (stats == "minMax") {
    results <- specStats_min_max(spectra)
    ribbon <- geom_ribbon(aes(ymin=.data$min, ymax=.data$max), fill = ribbon.col)
  } else {
    results <- specStats_sd(spectra)
    ribbon <- geom_ribbon(aes(ymin=.data$mean-.data$sd, ymax=.data$mean+.data$sd), fill = ribbon.col)
  }

  data <- as.data.frame(cbind(spectra[[1]], results))
  names(data) <- c("freq", "first", colnames(results))

  return(
    ggplot2::ggplot(data, aes(x=.data$freq, y=.data$mean)) +
      ribbon +
      geom_line(colour=line.col) +
      labs(x="Frequency (kHz)", y="Amplitude") +
      theme( axis.text.y=element_blank(),
             axis.ticks.y=element_blank()
      )
  )
}

#' Amplitudes of a list of spectra as a matrix
#'
#' One row per frequency bin and one column per spectrum, so that a statistic
#' across the spectra is a summary of a row rather than a nested loop.
#'
#' @param spectra A list of spectra
#' @return A numeric matrix of amplitudes.
#' @noRd
.specAmplitudes <- function(spectra) {
  return(vapply(spectra, function(s) s[,2], numeric(nrow(spectra[[1]]))))
}

#' @importFrom stats sd
#'
specStats_sd <- function(spectra) {
  amp <- .specAmplitudes(spectra)
  return(cbind(sd=apply(amp, 1, sd), mean=rowMeans(amp)))
}

specStats_min_max <- function(spectra) {
  amp <- .specAmplitudes(spectra)
  return(cbind(min=apply(amp, 1, min), max=apply(amp, 1, max), mean=rowMeans(amp)))
}
