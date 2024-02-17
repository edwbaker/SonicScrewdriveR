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
  if (inherits(wave, c("Wave"))) {
    if (length(noise) == 1) {
      n <- tuneR::noise(
        noise,
        duration=length(wave@left),
        samp.rate=wave@samp.rate,
        bit=wave@bit,
        stereo=wave@stereo
      )
    } else {
      n <- tuneR::silence(duration=length(wave@left), samp.rate=wave@samp.rate, bit=wave@bit, stereo=wave@stereo)
      for (i in 1:length(noise)) {
        nw <- tuneR::noise(noise[i], duration=length(wave@left), samp.rate=wave@samp.rate, bit=wave@bit)
        n@left <- n@left + nw@left
        if (wave@stereo) {
          n@right <- n@right + nw@left
        }
      }
    }
    if (noise.ref=="file") {
      noise.max <- max(abs(c(wave@left, wave@right)))
    } else {
      noise.max <- (2^n@bit/2)-1
    }
    noise.level <- noise.max
    # Scale n by noise.level
    n@left <- n@left * noise.level / max(n@left)
    if (wave@stereo) {
      n@right <- n@right * noise.level / max(n@right)
    }

    noise.frac = 1 / (1/noise.ratio - 1)
    wave@left <- (wave@left + noise.frac * n@left) / (1 + noise.frac)
    if (wave@stereo) {
      wave@right <- (wave@right + noise.frac * n@right) / (1 + noise.frac)
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
  if (inherits(wave, c("WaveMC"))) {
    #ToDo
    return(wave)
  }
  if (all(sapply(wave, inherits, c("Wave", "WaveMC")))) {
    return(lapply(wave, generateNoise, noise, noise.add, noise.ratio, noise.ref, output))
  }
  stop("wave must be a Wave like object, or a list of such objects.")
}
