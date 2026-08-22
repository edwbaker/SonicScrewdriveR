#' Correlate channels in a WaveMC object
#'
#' Uses the corenv function from seewave to calculate the envelope correlation for timed
#' events between the channels of a WaveMC object
#'
#' @details
#' The delays are the offsets `corenv()` reports, with the sign changed so that a
#' positive delay means the sound arrived at that channel later than it arrived
#' at the first channel, which is the convention `tdoa()` and `bearing()` use.
#' `corenv()` resolves an offset only to a whole sample, and reports any delay of
#' zero or less as one sample greater than it is. `tdoa()` with
#' `method="envelope"` measures the same thing without either limitation, and its
#' output can be passed to `bearing()`.
#'
#' `corenv()` correlates ranks rather than values unless it is told otherwise,
#' which for a recording with quiet stretches between events can put the largest
#' correlation at no offset at all whatever the offset really is. Pass
#' `method="pearson"` if the delays look implausible.
#'
#' @param wave A WaveMC object
#' @param times One or more times of events to correlate
#' @param window Width of the window to correlate in seconds (centred on times)
#' @param cluster A cluster for parallel execution
#' @param ... Additional arguments passed to `seewave::corenv()`, e.g. `method`
#'   or the envelope smoothing arguments.
#' @return A list with one entry per event, each holding the `corenv` list for
#'   every channel in `correlations`, and the delay of every channel relative to
#'   the first in `delays` (see Details).
#' @seealso [tdoa()], which measures the time differences of arrival between
#'   channels, and [bearing()], which turns them into a direction.
#' @export
corWaveMC <- function(wave, times, window, cluster=NULL, ...) {
  validateIsWaveMC(wave)

  outtimes <- vector(mode="list", length=length(times))
  for (i in seq_along(times)) {
    start <- times[i]-window/2
    end <- times[i]+window/2
    #The reference channel is the same for every channel of an event, so it is
    #cut once here rather than again inside each call.
    reference <- cutw(wave[,1], from=start, to=end, output="Wave")
    if (is.null(cluster)) {
      offsets <- lapply(seq_len(wave@dim[2]), corWaveMCchannel, wave=wave, from=start, to=end, reference=reference, ...)
    } else {
      offsets <- parallel::parLapply(cluster, seq_len(wave@dim[2]), corWaveMCchannel, wave=wave, from=start, to=end, reference=reference, ...)
    }
    outtimes[[i]] <- list(
      correlations = offsets,
      #corenv() reports how far the channel has to be moved to line it up with
      #the reference, which is the opposite of how late the sound reached it.
      delays = vapply(offsets, function(o) -o$t, numeric(1))
    )
  }
  return(outtimes)
}

#' @importFrom seewave corenv
corWaveMCchannel <- function (channel2, wave, from, to, reference=NULL, ...){
  if (is.null(reference)) {
    reference <- cutw(wave[,1], from=from, to=to, output="Wave")
  }
  return(corenv(reference, cutw(wave[,channel2], from=from, to=to, output="Wave"), plot=FALSE, ...))
}
