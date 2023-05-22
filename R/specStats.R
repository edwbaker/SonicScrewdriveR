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
#' @importFrom ggplot2 aes element_blank geom_line geom_ribbon labs theme
#' @export
#'
specStats <- function(spectra, stats="minMax", line.col="black", ribbon.col="grey70") {
  #Deal with other input types
  if (typeof(spectra) == "list") {
    validateSpectrum(spectra[[1]])
    for (i in 2:length(spectra)) {
      validateComparableSpectra(spectra[[1]], spectra[[i]])
      if (stats=="minMax") {
        results <- specStats_min_max(spectra)
        data <- as.data.frame(cbind(spectra[[1]], results))
        names(data) <- c("freq","first", "min", "max", "mean")
        plot <- ggplot2::ggplot(data, aes(x=data$freq, y=data$mean)) +
          geom_ribbon(aes(ymin=min,ymax=max), fill = ribbon.col) +
          geom_line(colour=line.col) +
          labs(x="Frequency (kHz)", y="Amplitude") +
          theme( axis.text.y=element_blank(),
                 axis.ticks.y=element_blank()
          )
      }
      if (stats=="sd") {
        results <- specStats_sd(spectra)
        data <- as.data.frame(cbind(spectra[[1]], results))
        names(data) <- c("freq","first", "sd", "mean")
        plot <- ggplot2::ggplot(data, aes(x=data$freq, y=data$mean)) +
          geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd), fill = ribbon.col) +
          geom_line(colour=line.col) +
          labs(x="Frequency (kHz)", y="Amplitude") +
          theme( axis.text.y=element_blank(),
                 axis.ticks.y=element_blank()
          )
      }}
  }
  print(plot)
  return(plot)
}


#' @importFrom stats sd
#'
specStats_sd <- function(spectra) {
  sd  <- vector(length=length(spectra[[1]][,1]))
  mean <- vector(length=length(spectra[[1]][,1]))
  for (i in 1:length(spectra[[1]][,1])) {
    values <- vector(length=length(spectra))
    for (j in 1:length(spectra)) {
      values[j] <- spectra[[j]][[i,2]]
    }
    sd[[i]] <- sd(values)
    mean[[i]]<- mean(values)
  }
  data <- cbind(sd, mean)
  return(data)
}

specStats_min_max <- function(spectra) {
  min  <- vector(length=length(spectra[[1]][,1]))
  max  <- vector(length=length(spectra[[1]][,1]))
  mean <- vector(length=length(spectra[[1]][,1]))
  for (i in 1:length(spectra[[1]][,1])) {
    values <- vector(length=length(spectra))
    for (j in 1:length(spectra)) {
      values[j] <- spectra[[j]][[i,2]]
    }
    min[[i]] <- min(values)
    max[[i]] <- max(values)
    mean[[i]]<- mean(values)
  }
  data <- cbind(min, max, mean)
  return(data)
}
