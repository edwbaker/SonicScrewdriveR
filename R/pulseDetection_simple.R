#' Simplified pulse detection using Dietrich (2004)
#'
#' Detects pulses in a Wave.
#'
#' @details
#' Onsets and offsets follow the convention described under [pulseDetection()]:
#' an onset is the first sample of a pulse and an offset the first sample after
#' it, so `offset - onset` is the length of the pulse in samples.
#'
#' The detection thresholds a windowed energy, which rises over the half window
#' before a pulse begins and falls over the half window after it ends, so each
#' boundary is displaced outward by part of a half window and a duration is
#' overstated by part of a whole one. For a square pulse of 400 samples at the
#' default `U` of 120 the duration comes back as 492 rather than 400. Two pulses
#' separated by less than about `U` samples are reported as one, because the
#' energy does not fall below the threshold between them. Both effects shrink
#' with `U`, which is the time resolution the detection is asked for.
#'
#' ste() computes a window only for the samples that hold a whole one, so the
#' half window at each end of the wave is not analysed and a pulse there is not
#' reported. Where both windows are wanted, as they are here, the longer of `U`
#' and `V` sets how much of each end is unanalysed.
#'
#' @param wave A Wave object
#' @param U Window length
#' @param gamma Gamma
#' @param alpha Alpha
#' @param scaling Scaling
#' @param V V Window length
#' @param psi Psi
#' @return A list of input values plus the onset and offset times of pulses.
#' @export
#'
pd_simple <- function(
  wave, 
  U=120,
  gamma=0.05, 
  alpha=1.4,
  scaling=32,
  V=480,
  psi=1
){
  e_u <- ste(wave, U=U)
  #ste() checks the format of the wave, and has just done so.
  e_v <- .withoutFormatWarning(ste(wave, U=V))
  #Only the samples both windows cover can be thresholded: ste() leaves a zero
  #where its window does not fit, and both e_u and e_v are read below. A wave
  #with no samples, or one shorter than the longer window, gives no short term
  #energy to threshold, so theta is undefined rather than nothing.
  analysed <- .steDefined(length(wave), max(U, V))
  if (length(analysed) == 0) {
    theta <- NA_real_
  } else {
    #theta is meant to sit a fraction gamma of the way up from the noise floor.
    #Taking it from the whole of e_u read the zeroes ste() leaves at each end as
    #the quietest energy in the recording, so the floor was zero however loud the
    #recording was: theta collapsed to gamma*max(e_u) and could not rise to the
    #real floor, which reported stationary noise as one pulse spanning the whole
    #wave and put its boundaries at the edges of the unmeasured half windows.
    energy <- e_u[analysed]
    theta <- min(energy) + gamma*(max(energy) - min(energy))
  }
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  #Nothing outside the analysed samples is a pulse, as the energy there is not a
  #windowed energy of this wave.
  pulse <- vector(mode="logical", length=length(wave))
  pulse[analysed] <- e_u[analysed] > F_a[analysed]
  #Reporting the sample before the pulse as its onset, which is what the rise in
  #pulse gave, left an onset that pulse itself does not mark as a pulse sample,
  #and disagreed by one sample with the offsets beside it and with the other two
  #detectors.
  runs <- .pulseRuns(pulse[analysed])

  #F_a is the threshold the pulses were found with, so it is returned alongside
  #F_1. Returning only F_1 meant the threshold given back did not reproduce the
  #pulses given back. The names match pd_dietrich2004().
  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    pulse = pulse,
    onsets = analysed[runs$onsets],
    offsets = analysed[runs$offsets]
  ))
}
