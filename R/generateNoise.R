#' Add noise to a soundwave
#'
#' Adding noise to a soundwave allows for testing of the robustness of automated identification algorithms
#' to noise.
#'
#' @param wave Wave file to add noise to
#' @param noise Vector of noise to add (unif, gaussian, white, pink, power, red, frequency of a sine wave in Hz, or filename)
#' @param noiseAdd If TRUE all noise sources are added to wave. If FALSE separate outputs are created for each noise source.
#' @param noiseRatio Ratio of maximum noise amplitude to the maximum amplitude in wave
#' @param output TODO: Is this implemented?
#' @param plot If TRUE various plots are made to show how noise is added.
#' @return A list of Wave objects with the required noise added.
#' @export
#'
generateNoise <- function(
  wave,
  noise = c("white"),
  noiseAdd = FALSE,
  noiseRatio=0.5,
  output = "file",
  plot=FALSE) {

  validateIsWave(wave)

  noiseComponents <- noise
  if (noiseAdd) {
    noise <- paste(noise, collapse="+")
  }

  data <- list()

  source <- tuneR::mono(wave)
  source <- tuneR::normalize(source)
  source_d <- seewave::duration(source)
  if (plot==TRUE) {
    seewave::oscillo(source)
  }
  for (j in 1:length(noise)) {
    noises <- tuneR::silence(duration=source_d, samp.rate = source@samp.rate, bit = source@bit, pcm=source@pcm, xunit="time")
    for (m in 1:length(noiseComponents)) {
      if (noiseComponents[[m]] %in% c("unif", "gaussian")) {
        n <- seewave::noisew(source@samp.rate, source_d, type=noiseComponents[[m]], output="Wave")
        n <- tuneR::normalize(n)
        noises <- noises + n
        next()
      }
      if (noiseComponents[[m]] %in% c("white", "pink", "power", "red")) {
        n <- tuneR::noise(kind=noiseComponents[[m]], duration=source_d, samp.rate=source@samp.rate, bit=source@bit, pcm=source@pcm, xunit="time")
        n <- tuneR::normalize(n)
        noises <- noises + n
        next()
      }
      if (noiseComponents[[m]] %in% c("rain", "thunder", "wind")) {

        next()
      }
      if (is.numeric(noiseComponents[[m]])) {
        n <- tuneR::sine(noiseComponents[[m]], duration=source_d, samp.rate=source@samp.rate, bit=source@bit, pcm=source@pcm, xunit="time")
        n <- tuneR::normalize(n)
        noises <- noises + n
        next()
      }
      if (file.exists(noiseComponents[[m]])) {
        nf <- tuneR::readWave(noiseComponents[[m]], from=0, to=source_d, units="seconds")
        nf <- tuneR::normalize(nf)
        nf_d <- seewave::duration(nf)

        n <- nf
        while (seewave::duration(n) < source_d) {
          n <- tuneR::bind(n, nf)
        }
        n <- seewave::cutw(n, nf@samp.rate, from=0, to=source_d, output="Wave")

        noises <- noises + n
        next()

      }

    }
    noises <-tuneR::normalize(noises)
    if (plot==TRUE) {
      seewave::oscillo(noises)
    }
    for (k in 1:length(noiseRatio)) {
      ratioNoise <- source + noises * noiseRatio[[k]]
      if (plot==TRUE) {
        seewave::oscillo(ratioNoise)
      }

      row <- list(noise=noise[[j]], noiseRatio=noiseRatio[[k]], wave=ratioNoise)

      data <- c(data, list(row))
    }
  }
  return(data)
}
