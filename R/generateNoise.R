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
  if (length(noise) == 1) {
    n <- tuneR::noise(
      noise,
      duration=duration,
      samp.rate=wave@samp.rate,
      bit=wave@bit
    )
  } else {
    n <- tuneR::silence(duration=duration, samp.rate=wave@samp.rate, bit=wave@bit)
    for (i in 1:length(noise)) {
      nw <- tuneR::noise(noise[i], duration=duration, samp.rate=wave@samp.rate, bit=wave@bit)
      n@left <- n@left + nw@left
    }
  }
  if (noise.ref=="file") {
    if (inherits(wave, "Wave")) {
      noise.max <- max(abs(c(wave@left, wave@right)))
    }
    if (inherits(wave, "WaveMC")) {
      noise.max <- max(abs(wave@.Data))
    }
  } else {
    noise.max <- (2^n@bit/2)-1
  }
  noise.level <- noise.max
  # Scale n by noise.level
  n@left <- n@left * noise.level / max(n@left)
  noise.frac = 1 / (1/noise.ratio - 1)

  if (inherits(wave, c("Wave"))) {
    wave@left <- (wave@left + noise.frac * n@left) / (1 + noise.frac)
    if (wave@stereo) {
      wave@right <- (wave@right + noise.frac * n@left) / (1 + noise.frac)
    }
  }
  if (inherits(wave, c("WaveMC"))) {
    for (i in 1:ncol(wave@.Data)) {
      wave@.Data[,i] <- (wave@.Data[,i] + noise.frac * n@left) / (1 + noise.frac)
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
