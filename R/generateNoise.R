#' Add noise to a Wave like object
#'
#' Adding noise to a Wave like object allows for testing of the robustness of
#' automated identification algorithms to noise.
#'
#' The noise added to each channel is generated separately, so the channels of a
#' multi-channel wave hold independent noise. One sequence used to be added to
#' every channel, which left the noise perfectly correlated between them and so
#' cancelling exactly in any difference between the channels: [tdoa()],
#' [bearing()] and [corWaveMC()] all read inter-channel differences, and noise
#' identical in every channel tests none of them.
#'
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()].
#'
#' @param wave Object to add noise to (`Wave`, `WaveMC`, or Tagged versions), or
#'   a list of such objects.
#' @param noise Vector of noise to add (unif, gaussian, white, pink, power, red)
#' @param noise.add If TRUE, the default, the noise sources are summed and the
#'   sum added to the wave. If FALSE each source is added to the wave on its own,
#'   and a list of waves is returned, one for each source.
#' @param noise.ratio Ratio of maximum noise amplitude to the maximum amplitude in wave.
#' @param noise.ref Reference maximum for noise.ratio. If "max" then the maximum
#'   amplitude, if "file" then the maximum amplitude of wave.
#' @param output TODO: Is this implemented?
#' @return The wave with the noise added, or a list of waves: one for each noise
#'   source where `noise.add` is FALSE and there is more than one, and one for
#'   each wave where a list of waves is given.
#' @export
#'
generateNoise <- function(
  wave,
  noise = c("white"),
  noise.add = TRUE,
  noise.ratio=0.5,
  noise.ref="file",
  output = "list"
){
  if (!inherits(wave, c("Wave", "WaveMC")) & !is(wave, "list")) {
    stop("wave must be a Wave like object, or a list of such objects.")
  }
  if (is(wave, "list")) {
    if (all(sapply(wave, inherits, c("Wave", "WaveMC")))) {
      return(lapply(wave, generateNoise, noise, noise.add, noise.ratio, noise.ref, output))
    } else {
      stop("wave must be a Wave like object, or a list of such objects.")
    }
  }
  #noise.add was never read, so several sources were always summed, while the
  #documented default of FALSE promised an output for each. The default is now
  #TRUE, which is what every call already got, and FALSE gives what it promised:
  #each source added on its own, and recorded as a call of its own.
  if (!noise.add && length(noise) > 1) {
    return(lapply(noise, function(n) generateNoise(wave, n, TRUE, noise.ratio, noise.ref, output)))
  }
  return(.tagApplyOne(
    wave, .generateNoise, "generateNoise", NULL, FALSE,
    noise=noise, noise.add=noise.add, noise.ratio=noise.ratio,
    noise.ref=noise.ref, output=output
  ))
}

#' Add noise to a wave without recording it
#' @keywords internal
#' @noRd
.generateNoise <- function(wave, noise, noise.add, noise.ratio, noise.ref, output) {
  duration <- length(wave)
  #tuneR::noise() always returns a floating point wave, which may only be 32 or
  #64 bit, so it is built at 32 rather than at the resolution of the wave the
  #noise is for. Giving it the resolution of a 16 bit recording was an error.
  #Only the samples of the noise are used, and they are scaled to the wave
  #below, so its resolution does not reach the result.
  noise.bit <- 32
  wave.offset <- if (wave@bit == 8) 128 else 0
  #The reference is a peak either side of that centre. .waveFullScale() gives
  #the span of the format, which for 8 bit is the whole unsigned 0 to 255 rather
  #than the 127 either side of 128, and scaling noise to the span sent an 8 bit
  #wave outside the range it can hold.
  wave.amplitude <- if (wave@bit == 8) 127 else .waveFullScale(wave)
  if (noise.ref=="file") {
    if (inherits(wave, "Wave")) {
      noise.max <- max(abs(c(wave@left, wave@right) - wave.offset))
    }
    if (inherits(wave, "WaveMC")) {
      noise.max <- max(abs(wave@.Data - wave.offset))
    }
    #An unsigned 8 bit wave reaches 128 below its centre but only 127 above it,
    #so a wave holding a zero gives a reference of 128 and the loudest noise
    #then takes the loudest sample past the end of the range.
    noise.max <- min(noise.max, wave.amplitude)
  } else {
    #The noise is added to the samples of the wave, so full scale is the wave's,
    #not the noise's. The noise is always floating point, which is full scale at
    #one, and taking the reference from it made the noise added to a 16 bit
    #recording thirty thousand times quieter than asked for.
    noise.max <- wave.amplitude
  }
  noise.level <- noise.max
  #The reference is taken over the whole wave, so every channel is given noise
  #at the same level, but each channel is given a sequence of its own. One
  #sequence was generated and added to every channel, which left the noise
  #identical between them: it cancelled exactly in any difference between
  #channels, and tdoa(), bearing() and corWaveMC() all read those differences.
  channelNoise <- function() {
    .noiseSamples(noise, duration, wave@samp.rate, noise.bit, noise.level)
  }
  #The mix is written as a weighted sum of signal and noise rather than through
  #noise.frac = 1/(1/noise.ratio - 1). The two are identical for every ratio
  #below one - with f the fraction, (x + f*n)/(1 + f) is (1-r)*x + r*n - but
  #noise.frac is Inf at noise.ratio=1 and (x + Inf*n)/(1 + Inf) is Inf/Inf, so
  #every sample came back NaN. Written this way the endpoint is the limit the
  #arithmetic implies: all noise and none of the signal.
  signal.weight <- 1 - noise.ratio

  if (inherits(wave, c("Wave"))) {
    wave@left <- signal.weight * (wave@left - wave.offset) + noise.ratio * channelNoise() + wave.offset
    if (wave@stereo) {
      wave@right <- signal.weight * (wave@right - wave.offset) + noise.ratio * channelNoise() + wave.offset
    }
  }
  if (inherits(wave, c("WaveMC"))) {
    for (i in 1:ncol(wave@.Data)) {
      wave@.Data[,i] <- signal.weight * (wave@.Data[,i] - wave.offset) + noise.ratio * channelNoise() + wave.offset
    }
  }
  #The weighted sum is not whole, and a PCM wave holding it declared a format
  #its samples did not fit, so it is rounded here, as every function that puts
  #samples back in a wave rounds them. Every sum lies within full scale, which
  #is whole, so rounding cannot take a sample past it.
  if (isTRUE(wave@pcm)) {
    if (inherits(wave, "WaveMC")) {
      wave@.Data <- round(wave@.Data)
    } else {
      wave@left <- round(wave@left)
      if (wave@stereo) {
        wave@right <- round(wave@right)
      }
    }
  }
  return(wave)
}

#' One channel's worth of noise, scaled to a level
#'
#' The noise sources asked for are summed and the sum scaled so that its
#' deepest sample either way sits at `level`. Each channel of a wave calls this
#' separately, so each is given noise of its own.
#'
#' @param noise Vector of noise types to sum.
#' @param duration Length in samples.
#' @param samp.rate Sample rate of the wave the noise is for.
#' @param bit Resolution to generate the noise at.
#' @param level The amplitude the noise is scaled to.
#' @return A numeric vector of samples.
#' @noRd
.noiseSamples <- function(noise, duration, samp.rate, bit, level) {
  if (length(noise) == 1) {
    n <- tuneR::noise(
      noise,
      duration=duration,
      samp.rate=samp.rate,
      bit=bit
    )
  } else {
    n <- tuneR::silence(duration=duration, samp.rate=samp.rate, bit=bit, pcm=FALSE)
    for (i in 1:length(noise)) {
      nw <- tuneR::noise(noise[i], duration=duration, samp.rate=samp.rate, bit=bit)
      n@left <- n@left + nw@left
    }
  }
  # Scale by the largest sample either way rather than the largest positive one.
  # Noise whose deepest trough is lower than its highest peak was scaled so that
  # the trough went past the level asked for, and with the level now being full
  # scale that is far enough to leave the format.
  return(n@left * level / max(abs(n@left)))
}
