#' Upsample a wave
#'
#' Used to upsample a Wave object. The upsampled sample rate must be an natural multiple
#' of the current sample rate.
#'
#' Applied to a `TaggedWave` the call is recorded in the `processing` slot of
#' the result, as described in [tagApply()].
#'
#' Interpolation between two samples lands between them, so where the wave
#' declares integer PCM the interpolated samples are rounded to whole numbers.
#' A floating point wave is interpolated without rounding.
#'
#' @param wave Wave object to upsample.
#' @param upsample.rate The sample rate to upsample to.
#' @param method "basic" for linear, or a function to interpolate NAs in a vector
#' @export
#' @return A resampled Wave object of the same format as the input
#' @importFrom tuneR sine
#' @examples
#' wave <- tuneR::sine(4000, samp.rate=44100)
#' wave2 <- upsample(wave, 88200)
#'
upsample <- function(wave, upsample.rate, method="basic") {
  validateIsWave(wave)
  sf <- upsample.rate / wave@samp.rate
  if (sf != round(sf)) {
    stop("Scale factor is not an integer.")
  }
  return(.tagApplyOne(
    wave, .upsample, "upsample", NULL, FALSE,
    upsample.rate=upsample.rate, method=method
  ))
}

#' Upsample a wave without recording it
#' @keywords internal
#' @noRd
.upsample <- function(wave, upsample.rate, method) {
  sf <- upsample.rate / wave@samp.rate
  samples <- .upsampleChannel(wave@left, sf, method)
  if (wave@stereo) {
    samples <- cbind(samples, .upsampleChannel(wave@right, sf, method))
  }
  #Interpolating between two samples of an integer PCM wave lands between them,
  #so .rebuildWave() rounds them to the format the wave declares. Rounding cannot
  #carry a sample outside the range the input held. Linear interpolation returns
  #convex combinations of two neighbouring samples, and the final sample is held
  #rather than extrapolated, so every value already lies between two values the
  #input held - including eight bit, where PCM is unsigned and runs from 0 to
  #255 rather than either side of zero.
  return(.rebuildWave(samples, wave, samp.rate=upsample.rate))
}

.upsampleChannel <- function(channel, sf, method) {
  n <- length(channel)

  if (is.character(method) && length(method) == 1 && method == "basic" && sf > 1) {
    #The final sample has no following sample to interpolate towards, so its
    #value is held rather than producing NAs at the end of the channel.
    d <- c(diff(channel), 0) / sf
    #Each row holds one input sample and the sf-1 interpolated samples that follow
    #it, so transposing gives the channel in order.
    return(as.vector(t(channel + outer(d, 0:(sf-1)))))
  }

  #Other methods are expected to interpolate the NAs left between input samples.
  new <- rep.int(NA_real_, n*sf)
  new[seq.int(from=1, by=sf, length.out=n)] <- channel

  #The documented use of method is a function that fills those NAs. It was never
  #called, so any method other than "basic" returned a channel of NAs.
  if (is.function(method)) {
    return(method(new))
  }
  if (!identical(method, "basic")) {
    stop(paste("Unknown method for upsample:", method))
  }
  return(new)
}
