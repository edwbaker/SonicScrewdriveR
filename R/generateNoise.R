#' Add noise to a Wave like object
#'
#' Adding noise to a Wave like object allows for testing of the robustness of
#' automated identification algorithms to noise.
#'
#' @param wave Object to add noise to (`Wave`, `WaveMC`, or Tagged versions), or
#'   a list of such objects.
#' @param noise Vector of noise to add (unif, gaussian, white, pink, power, red)
#' @param noiseAdd If TRUE all noise sources are added to wave. If FALSE
#'   separate outputs are created for each noise source.
#' @param noiseRatio Ratio of maximum noise amplitude to the maximum amplitude in wave.
#' @param output TODO: Is this implemented?
#' @return A list of Wave objects with the required noise added.
#' @export
#'
generateNoise <- function(
  wave,
  noise = c("white"),
  noiseAdd = FALSE,
  noiseRatio=0.5,
  output = "list"
){
  if (inherits(wave, c("Wave", "WaveMC"))) {
    if (output == "list") {
      return(wave)
    }
  }
  if (all(sapply(wave, inherits, c("Wave", "WaveMC")))) {
    data <- lapply(wave, generateNoise, noise, noiseAdd, noiseRatio, output)
    return(data)
  }
  stop("wave must be a Wave like object, or a list of such objects.")
}
