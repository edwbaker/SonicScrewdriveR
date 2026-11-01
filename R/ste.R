#' Short term energy
#'
#' Computes the short term energy of a Wave.
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which returns the energy of each channel separately.
#' @param method Which method used to calculate the short term energy,
#' by default "dietrich2004" to use \insertCite{dietrich2004}{sonicscrewdriver}.
#' @param U Window length in samples, matching the pulse detection functions.
#' @param ... Other arguments to pass to ste method.
#' @references
#'   \insertAllCited{}
#' @export
#' @return A vector of short term energy values. For multi-channel input, one
#'   such vector per channel.
#' @examples
#' \dontrun{
#' ste(sheep, method="dietrich2004")
#' }
#'
ste <-  function(
  wave,
  method="dietrich2004",
  U=120,
  ...
){
  #The method is checked before the channels are split so that an unknown one is
  #reported once rather than once per channel.
  .validateChoice(method, c("dietrich2004"), "method", "ste", prep="for")
  #A stereo Wave read only its left channel, so the energy of the right one was
  #never reported, and a WaveMC has no left slot to read at all.
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(wave, function(w) ste(w, method=method, U=U, ...), channel.param=NULL))
  }
  #Nothing here checked the class, so anything that was not a wave reached the
  #left slot and failed there, naming a slot rather than the argument.
  validateIsWave(wave)
  return(.ste_dietrich2004(wave, U=U, ...))
}

.ste_dietrich2004 <- function(wave, U=120) {
  # The energy at sample i is the sum of abs(wave@left) over a window of U+1
  # samples centred on i, so a cumulative sum gives every value in one pass.
  # For odd U the window sits half a sample to the left of centre.
  x <- abs(wave@left)
  n <- length(wave)
  before <- floor(U/2)

  e <- vector(mode="numeric", length=n)
  i <- .steDefined(n, U)
  if (length(i) == 0) {
    return(e)
  }

  cs <- c(0, cumsum(x))
  e[i] <- cs[i - before + U + 1] - cs[i - before]
  return(e)
}

#' The samples for which ste() computes a windowed energy
#'
#' ste() centres a window of U+1 samples on each sample, so the half window at
#' each end of a wave holds no window of its own and is left as a zero rather
#' than as an energy. This gives the samples that do hold one, which are the only
#' samples the energy-based pulse detectors can threshold: the zeroes are not the
#' quietest part of the recording but the part of it that was not measured.
#'
#' @param n The number of samples in the wave.
#' @param U Window length in samples.
#' @return An increasing vector of sample positions, empty where no window fits.
#' @noRd
.steDefined <- function(n, U) {
  before <- floor(U/2)
  after <- ceiling(U/2)
  if (n - after < before + 1) {
    return(integer(0))
  }
  return((before + 1):(n - after))
}
