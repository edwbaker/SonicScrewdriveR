#' Correlate channels in a WaveMC object
#'
#' Uses the corenv function from seewave to calculate the envelope correlation for timed
#' events between the channels of a WaveMC object
#' @param wave A WaveMC object
#' @param times One or more times of events to correlate
#' @param window Width of the window to correlate in seconds (centred on times)
#' @param temp Air temperature in Celsius
#' @param cluster A cluster for parallel execution
#' @return List of corenv lists for events, and a list of the time differences between channels
#' @export
corWaveMC <- function(wave, times, window, temp=25, cluster=NULL) {
  validateIsWaveMC(wave)

  outtimes <- vector(mode="list", length=length(times))
  for (i in 1:length(times)) {
    offsets <- vector(mode="list", length=wave@dim[2])
    delays <- vector(mode="numeric", length=wave@dim[2])
    start <- times[i]-window/2
    end <- times[i]+window/2
    if (is.null(cluster)) {
      offsets <- lapply(1:wave@dim[2], corWaveMCchannel, wave=wave, from=start, to=end)
    } else {
      offsets <- parallel::parLapply(cluster, 1:wave@dim[2], corWaveMCchannel, wave=wave, from=start, to=end)
    }
    outtimes[[i]] <- offsets
  }
  return(outtimes)
}

#' @importFrom seewave corenv
corWaveMCchannel <- function (channel2, wave, from, to){
  ret <- corenv(cutw(wave[,1],from=from,to=to, output="Wave"), cutw(wave[,channel2],from=from,to=to, output="Wave"),plot=F)
  return(ret)
}
