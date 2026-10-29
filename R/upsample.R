#' Upsample a wave
#'
#' Used to upsample a Wave object. The upsampled sample rate must be an natural multiple
#' of the current sample rate.
#'
#' Applied to a `TaggedWave` the call is recorded in the `processing` slot of
#' the result, as described in [tagApply()].
#'
#' @param wave Wave object to upsample.
#' @param upsample.rate The sample rate to upsample to.
#' @param method "basic" for linear, or a function to interpolate NAs in a vector
#' @export
#' @return A resampled Wave object
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
  newleft <- .upsampleChannel(wave@left, sf, method)

  if (wave@stereo) {
    newright <- .upsampleChannel(wave@right, sf, method)
    return(stereo(
      Wave(newleft, samp.rate=upsample.rate, bit=wave@bit, pcm=wave@pcm),
      Wave(newright, samp.rate=upsample.rate, bit=wave@bit, pcm=wave@pcm)))
  }

  return(Wave(newleft, samp.rate=upsample.rate, bit=wave@bit, pcm=wave@pcm))
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
