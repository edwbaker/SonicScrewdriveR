#' Correlate channels in a WaveMC object
#'
#' Uses the corenv function from seewave to calculate the envelope correlation for timed
#' events between the channels of a WaveMC object
#' @param wave A WaveMC object
#' @param times One or more times of events to correlate
#' @param window Width of the window to correlate in seconds (centered on times)
#' @param temp Air temperature in Celsius
#' @return List of corenv lists for events, and a list of the time differences between channels
#' @export
#' @importFrom seewave corenv
corWaveMC <- function(wave, times, window, temp=25) {
  #TODO: validate is waveMC

  for (i in 1:length(times)) {
    offsets <- vector(mode="list", length=wave@dim[2])
    delays <- vector(mode="numeric", length=wave@dim[2])
    #Todo: convert to parallel apply
    for (c in 1:wave@dim[2]) {
      start <- times[i] - window/2
      end <- times[i] + window/2
      offsets[[c]] <- corenv(cutw(wave[,1],from=start,to=end, output="Wave"), cutw(wave[,c],from=start,to=end, output="Wave"), plot=F)
      delays[c] <- offsets[[c]]$t
    }
  }
  data <- list(
    "raw" = offsets,
    "times" = delays
  )
  return(data)
}
