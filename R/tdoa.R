#' Time differences of arrival between channels
#'
#' Estimates the time difference of arrival (TDOA) of a sound between the
#' channels of a multichannel recording, by generalised cross-correlation of each
#' channel against a reference channel.
#'
#' @details
#' A positive delay means the sound arrived at that channel later than it arrived
#' at the reference channel, so the reference channel is the closer of the two to
#' the source. The delay of the reference channel against itself is zero.
#'
#' Cross-correlation is performed over the whole recording unless `from` and `to`
#' are given, and a single delay is returned for each channel. Recordings holding
#' more than one event should be cut to each event in turn (see Examples), as a
#' correlation over several events with different directions of arrival has no
#' single peak to find. Cutting is also much the faster way to work, as the whole
#' of whatever is given is transformed at once.
#'
#' The following methods are available:
#' * **phat** (the default) - the phase transform, in which each frequency
#'   contributes to the correlation according to its phase alone
#'   \insertCite{knapp1976}{sonicscrewdriver}. Discarding the magnitudes
#'   sharpens the correlation peak and makes the estimate more robust to
#'   reverberation, at the cost of weighting quiet frequencies (which are mostly
#'   noise) as heavily as loud ones.
#' * **cc** - unweighted cross-correlation, normalised so that the returned
#'   correlation is between -1 and 1. Better suited to recordings with a poor
#'   signal to noise ratio, and to narrowband sounds.
#' * **envelope** - cross-correlation of the Hilbert amplitude envelopes of the
#'   channels rather than of the waveforms themselves, as `seewave::corenv()`
#'   does. The only method of the three that works on a sound whose waveform does
#'   not stay coherent between microphones, which is the usual situation for the
#'   broadband pulses of insects and for microphones far enough apart that the
#'   path to each differs. Delays are resolved only to the width of a feature of
#'   the envelope, so it is the least precise of the three where the others work
#'   at all.
#'
#' The delay found is a whole number of samples, which at the sample rates used
#' for field recording is a coarse measure of direction. `interpolate` fits a
#' parabola to the correlation peak and its two neighbours to estimate the delay
#' to a fraction of a sample.
#'
#' @param wave A Wave or WaveMC object with two or more channels.
#' @param ref The channel that delays are measured against.
#' @param from Optionally, the start of the region to correlate.
#' @param to Optionally, the end of the region to correlate.
#' @param units Units in which `from` and `to` are given, one of "samples",
#'   "seconds" (the default), "minutes", or "hours".
#' @param method Correlation method, one of "phat" (the default), "cc", or
#'   "envelope" (see Details).
#' @param max.delay Optionally, the largest delay to consider, in seconds. Delays
#'   larger than the time sound takes to cross the microphone array are not
#'   physically possible, and excluding them stops a spurious correlation peak
#'   being returned.
#' @param interpolate If TRUE (the default) the delay is interpolated to a
#'   fraction of a sample (see Details).
#' @return A data frame with one row per channel, holding the channel number, the
#'   channel name (NA if the channels are not named), the delay in seconds, the
#'   delay in samples, and the height of the correlation peak.
#' @references
#'   \insertAllCited{}
#' @export
#' @importFrom stats fft nextn
#' @importFrom tuneR noise
#' @seealso [bearing()], which turns the delays into a direction.
#' @examples
#' # A noise reaching the second channel of a recording 20 samples after
#' # it reaches the first.
#' n <- tuneR::noise(kind="white", duration=5000, samp.rate=48000)
#' w <- tuneR::WaveMC(
#'   cbind(n@left[101:4900], n@left[81:4880]),
#'   samp.rate = 48000,
#'   bit = 32,
#'   pcm = FALSE
#' )
#' tdoa(w)
#'
#' # Only the middle of the recording, and only delays that an array 1m across
#' # could produce.
#' tdoa(w, from=0.02, to=0.08, max.delay=1/soundSpeed())
#'
#' \dontrun{
#' # Delays for each of several events in a recording, each 100ms long
#' w <- readAudio("array.wav")
#' events <- c(3.2, 8.7, 12.1)
#' lapply(events, function(t) tdoa(w, from=t, to=t+0.1))
#' }
#'
tdoa <- function(
  wave,
  ref=1,
  from=NULL,
  to=NULL,
  units="seconds",
  method="phat",
  max.delay=NULL,
  interpolate=TRUE
) {
  validateIsWaveLike(wave)
  wave <- untagWave(wave)
  if (!method %in% c("phat", "cc", "envelope")) {
    stop(paste("Unknown method for tdoa:", method))
  }
  if (!is.null(from) | !is.null(to)) {
    wave <- .cutRegion(wave, from, to, units)
  }
  d <- .channelMatrix(wave)
  channels <- ncol(d)
  if (channels < 2) {
    stop("Time differences of arrival need a wave with two or more channels.")
  }
  if (length(ref) != 1 || !ref %in% seq_len(channels)) {
    stop(paste0("ref must be a channel of the wave (1 to ", channels, ")."))
  }
  if (nrow(d) < 2) {
    stop("The region to correlate is too short.")
  }
  channel.names <- colnames(d)
  if (method == "envelope") {
    d <- .channelEnvelopes(d, wave@samp.rate)
  }

  max.lag <- nrow(d) - 1
  if (!is.null(max.delay)) {
    if (!is.numeric(max.delay) | max.delay <= 0) {
      stop("max.delay must be a positive number of seconds.")
    }
    max.lag <- min(max.lag, floor(max.delay * wave@samp.rate))
    if (max.lag < 1) {
      stop("max.delay is shorter than one sample.")
    }
  }

  #The envelopes are correlated unweighted: a phase transform would weight the
  #frequencies an envelope has least of, which is most of them, as heavily as the
  #few it has.
  correlation <- if (method == "envelope") "cc" else method
  peaks <- lapply(
    seq_len(channels),
    function(i) .gcc(d[,ref], d[,i], method=correlation, max.lag=max.lag, interpolate=interpolate)
  )

  if (is.null(channel.names)) {
    channel.names <- rep(NA_character_, channels)
  }
  samples <- vapply(peaks, function(p) p$lag, numeric(1))
  return(data.frame(
    channel = seq_len(channels),
    name = channel.names,
    delay = samples / wave@samp.rate,
    samples = samples,
    r = vapply(peaks, function(p) p$r, numeric(1)),
    stringsAsFactors = FALSE
  ))
}

#' Hilbert amplitude envelope of every channel
#'
#' @param d A matrix with one column per channel.
#' @param samp.rate Sample rate of the channels.
#' @return A matrix of the same shape holding the envelope of each channel.
#' @keywords internal
#' @noRd
#' @importFrom seewave env
.channelEnvelopes <- function(d, samp.rate) {
  return(vapply(
    seq_len(ncol(d)),
    function(i) as.vector(env(d[,i], f=samp.rate, envt="hil", plot=FALSE)),
    numeric(nrow(d))
  ))
}

#' Generalised cross-correlation of two vectors
#'
#' @param x Reference vector.
#' @param y Vector to find the delay of.
#' @param method One of "phat" or "cc".
#' @param max.lag Largest lag to consider, in samples.
#' @param interpolate Interpolate the peak to a fraction of a sample.
#' @return A list holding the lag in samples and the height of the peak.
#' @keywords internal
#' @noRd
.gcc <- function(x, y, method="phat", max.lag=NULL, interpolate=TRUE) {
  n <- length(x)
  #Zero padded to at least twice the length, so that a delay is not wrapped
  #around the end of the correlation and returned as a delay of the opposite
  #sign.
  N <- nextn(2*n, factors=2)
  x <- x - mean(x)
  y <- y - mean(y)
  if (all(x == 0) | all(y == 0)) {
    return(list(lag=0, r=0))
  }
  X <- fft(c(x, rep(0, N-n)))
  Y <- fft(c(y, rep(0, N-n)))
  #Conjugating the reference puts the peak at the delay of y relative to x.
  R <- Y * Conj(X)
  if (method == "phat") {
    R <- R / pmax(Mod(R), .Machine$double.eps)
  }
  #fft() does not scale its inverse, so the division by N is what makes this a
  #correlation rather than N times one.
  r <- Re(fft(R, inverse=TRUE)) / N
  if (method == "cc") {
    r <- r / sqrt(sum(x^2) * sum(y^2))
  }

  #fft() returns the negative lags in the upper half of the vector. Reordering
  #them to run from most negative to most positive puts the neighbours of a peak
  #either side of it for interpolation.
  half <- N/2
  r <- c(r[(half+1):N], r[1:half])
  lags <- -half:(half-1)

  keep <- which(abs(lags) <= max.lag)
  peak <- keep[which.max(r[keep])]

  lag <- lags[peak]
  if (interpolate & peak > 1 & peak < length(r)) {
    lag <- lag + .parabolicPeak(r[peak-1], r[peak], r[peak+1])
  }
  return(list(lag=lag, r=r[peak]))
}

#' Offset of the vertex of a parabola through three equally spaced points
#'
#' @param left Value before the peak.
#' @param peak Value at the peak.
#' @param right Value after the peak.
#' @return The offset of the vertex from the middle point, between -0.5 and 0.5.
#' @keywords internal
#' @noRd
.parabolicPeak <- function(left, peak, right) {
  denominator <- left - 2*peak + right
  #A flat or upward curve has no peak between the neighbours to interpolate to.
  if (denominator >= 0) {
    return(0)
  }
  offset <- 0.5 * (left - right) / denominator
  return(max(-0.5, min(0.5, offset)))
}

#' Cut a region from a wave given in any supported time unit
#'
#' @param wave A Wave or WaveMC object.
#' @param from Start of the region, or NULL for the start of the wave.
#' @param to End of the region, or NULL for the end of the wave.
#' @param units Units in which from and to are given.
#' @return A Wave or WaveMC object.
#' @keywords internal
#' @noRd
.cutRegion <- function(wave, from=NULL, to=NULL, units="seconds") {
  if (units == "samples") {
    from <- if (is.null(from)) 1 else from
    to <- if (is.null(to)) length(wave) else to
  } else {
    from <- if (is.null(from)) 1 else convert2seconds(from, input=units) * wave@samp.rate + 1
    to <- if (is.null(to)) length(wave) else convert2seconds(to, input=units) * wave@samp.rate
  }
  from <- max(1, round(from))
  to <- min(length(wave), round(to))
  return(cutws(wave, from, to))
}
