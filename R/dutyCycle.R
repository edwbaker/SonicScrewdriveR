#' Calculate the duty cycle of a wave
#'
#' Proportion of a wave with signal above the limit
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which returns a list with one duty cycle per channel.
#' @param limit Threshold above which to consider the signal, as a proportion of
#'   the largest excursion from silence the wave's format can hold: the default
#'   of 0.1 is a tenth of full scale, whether the wave is floating point or
#'   integer, signed or unsigned. The threshold does not depend on the wave's own
#'   peak, so a loud transient does not change the duty cycle measured around it
#'   and the same sound recorded twice gives the same figure. In earlier releases
#'   this was a proportion of the loudest sample in the wave, which a value given
#'   explicitly still means only where that sample is at full scale.
#' @param output If "unit" the duty cycle will be in the range 0-1. For a percentage use "percent".
#' @param normalise If TRUE the Wave is normalised using tuneR before the
#'   threshold is applied. Normalising raises the wave's loudest sample to full
#'   scale, which makes the threshold depend on that sample again, and
#'   normalize() from tuneR removes the offset as well, so silence no longer sits
#'   at the centre of the format. The default is FALSE and was TRUE in earlier
#'   releases.
#' @return A numerical value for the duty cycle between 0 and 1 (or 0 and 100% if
#'   percentage output). For multi-channel input, a list of such values.
#' @examples
#' wave <- tuneR::sine(2000)
#' dc <- dutyCycle(wave)
#' pc <- dutyCycle(wave, output="percent")
#' @export
#'
dutyCycle <- function(
  wave,
  limit=0.1,
  output="unit",
  normalise = FALSE
) {
  .validateChoice(output, c("unit", "percent"), msg="output format not recognised.")
  #limit is a proportion of full scale, so a caller reading the help page as it
  #was and passing an amplitude - 3000 for a 16 bit recording - would be given a
  #duty cycle of zero rather than told the value is not a proportion.
  if (!is.numeric(limit) || length(limit) != 1) {
    stop("limit must be a single proportion of full scale, between 0 and 1.")
  }
  .validateNumericRange(
    limit, "limit", min=0, max=1,
    range.msg="limit must be a proportion of full scale, between 0 and 1."
  )
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(
      wave,
      function(w) dutyCycle(w, limit=limit, output=output, normalise=normalise),
      channel.param = NULL
    ))
  }
  #An NA sample reached validateDutyCycle() as an NA duty cycle and surfaced as
  #"missing value where TRUE/FALSE needed", which mentions neither NA nor the
  #wave. The check comes before normalise(), which would spread the NA over the
  #whole wave first.
  if (anyNA(wave@left)) {
    stop("Wave contains NA samples, so its duty cycle cannot be calculated.")
  }
  if (normalise) {
    wave <- normalise(wave)
  }
  #How far each sample is from silence, which is the centre of the format rather
  #than zero for the one format that is unsigned. Eight bit PCM runs 0 to 255
  #about a midpoint of 128, so abs() read its silence as 128 and every sample of
  #an eight bit wave cleared any threshold below half of full scale: a train
  #with a duty cycle of 0.1 measured 1 at every limit up to 0.5.
  w <- abs(wave@left - .waveCentre(wave))
  l <- length(w)
  #The threshold is a proportion of what the wave's format can hold rather than
  #of the loudest sample it holds. Taken from that sample, one transient set the
  #threshold for everything around it: a signal 40 dB down with a true duty
  #cycle of 0.1 measured 0.2503, and 0.0001 once a single sample reached full
  #scale, so no two recordings of one sound could be compared.
  c <- sum(w > limit * .waveFullExcursion(wave))
  if (output == "unit") {
    return(validateDutyCycle(c/l))
  }
  return(100*validateDutyCycle(c/l))
}
