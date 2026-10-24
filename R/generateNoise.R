#' Add noise to a Wave like object
#'
#' Adding noise to a Wave like object allows for testing of the robustness of
#' automated identification algorithms to noise.
#'
#' @param wave Object to add noise to (`Wave`, `WaveMC`, or Tagged versions), or
#'   a list of such objects.
#' @param noise Vector of noise to add (unif, gaussian, white, pink, power, red)
#' @param noise.add If TRUE all noise sources are added to wave. If FALSE
#'   separate outputs are created for each noise source.
#' @param noise.ratio Ratio of maximum noise amplitude to the maximum amplitude in wave.
#' @param noise.ref Reference maximum for noise.ratio. If "max" then the maximum
#'   amplitude, if "file" then the maximum amplitude of wave.
#' @param output TODO: Is this implemented?
#' @return A list of Wave objects with the required noise added.
#' @export
#'
generateNoise <- function(
  wave,
  noise = c("white"),
  noise.add = FALSE,
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
  duration <- length(wave)
  #tuneR::noise() always returns a floating point wave, which may only be 32 or
  #64 bit, so it is built at 32 rather than at the resolution of the wave the
  #noise is for. Giving it the resolution of a 16 bit recording was an error.
  #Only the samples of the noise are used, and they are scaled to the wave
  #below, so its resolution does not reach the result.
  noise.bit <- 32
  if (length(noise) == 1) {
    n <- tuneR::noise(
      noise,
      duration=duration,
      samp.rate=wave@samp.rate,
      bit=noise.bit
    )
  } else {
    n <- tuneR::silence(duration=duration, samp.rate=wave@samp.rate, bit=noise.bit, pcm=FALSE)
    for (i in 1:length(noise)) {
      nw <- tuneR::noise(noise[i], duration=duration, samp.rate=wave@samp.rate, bit=noise.bit)
      n@left <- n@left + nw@left
    }
  }
  wave.offset <- if (wave@bit == 8) 128 else 0
  if (noise.ref=="file") {
    if (inherits(wave, "Wave")) {
      noise.max <- max(abs(c(wave@left, wave@right) - wave.offset))
    }
    if (inherits(wave, "WaveMC")) {
      noise.max <- max(abs(wave@.Data - wave.offset))
    }
  } else {
    #The noise is added to the samples of the wave, so full scale is the wave's,
    #not the noise's. The noise is always floating point, which is full scale at
    #one, and taking the reference from it made the noise added to a 16 bit
    #recording thirty thousand times quieter than asked for.
    noise.max <- .waveFullScale(wave)
  }
  noise.level <- noise.max
  # Scale n by noise.level
  n@left <- n@left * noise.level / max(n@left)
  noise.frac = 1 / (1/noise.ratio - 1)

  if (inherits(wave, c("Wave"))) {
    wave@left <- (wave@left - wave.offset + noise.frac * n@left) / (1 + noise.frac) + wave.offset
    if (wave@stereo) {
      wave@right <- (wave@right - wave.offset + noise.frac * n@left) / (1 + noise.frac) + wave.offset
    }
  }
  if (inherits(wave, c("WaveMC"))) {
    for (i in 1:ncol(wave@.Data)) {
      wave@.Data[,i] <- (wave@.Data[,i] - wave.offset + noise.frac * n@left) / (1 + noise.frac) + wave.offset
    }
  }
  if (.isTagged(wave)) {
    wave <- addProcess(
      wave,
      "generateNoise",
      list(
        "noise" = noise,
        "noise.add" = noise.add,
        "noise.ratio" = noise.ratio,
        "noise.ref" = noise.ref
      )
    )
  }
  return(wave)
}
