#' Pulse detection using Dietrich (2004)
#'
#' Detects pulses in a Wave using the method described in Dietrich et al (2004) <doi:10.1016/j.patcog.2004.04.004>.
#'
#' @details
#' The short term energy is computed over a window of `U` samples (equation 5 of
#' the paper) and over a longer window of `V` samples, which gives a local
#' reference level. Two thresholds are derived from the long window energy
#' (equation 7), `F_1 = theta + e_v/scaling` and
#' `F_a = theta + alpha*e_v/scaling`, where `theta` is the lower energy bound of
#' equation 8. As `alpha` is greater than one, `F_a` is the upper of the two.
#'
#' Algorithm 1 of the paper then walks the wave a sample at a time. A candidate
#' pulse opens at the sample where the energy first rises above `F_1`, and is
#' confirmed only if the energy also rises above `F_a` within `psi` samples of
#' that crossing. The sample reported is the `F_1` crossing that opened the
#' candidate, not the `F_a` crossing that confirmed it. Where `F_a` is not
#' reached in time the candidate is abandoned and the next sample above `F_1`
#' opens a new one, so a larger `psi` lets a slow attack be reported from
#' further back. Once a pulse has been reported, no further pulse is reported
#' until the energy falls below `F_1` again.
#'
#' The paper says only that a similar algorithm estimates the offsets. Here the
#' same scan is run over the wave in reverse, so an offset closes a run above
#' `F_1` that `F_a` confirmed within `psi` samples of its end. Both boundaries
#' are therefore measured at `F_1` and a pulse length is its width at `F_1`.
#'
#' Section 2 is two steps, and the segmentation above is the second. The first,
#' done here unless `filter` is `FALSE`, divides the wave by its maximum norm
#' (equation 1), finds the frequency `omega` carrying the most energy, weighted
#' by a density `W` over the frequencies the sound can occupy (equation 2), and
#' removes everything further than `delta` from it (equations 3 and 4). This is
#' what frees the energy of the sounds of the environment before any of it is
#' thresholded. The energies and thresholds returned are then in the units of
#' the normalised signal rather than of the wave as supplied. Normalisation on
#' its own changes no boundary, as `theta`, `F_1` and `F_a` are all linear in
#' the amplitude of the wave.
#'
#' `band` is what keeps that first step useful, and it is worth setting for the
#' animal in hand. It gives the support of `W`, and so the range equation 2 is
#' allowed to choose from. Left to the whole spectrum, the loudest frequency of
#' a field recording is usually wind or handling noise well below the song, and
#' the filter then keeps that and discards the song: on a wave of 5 kHz pulses
#' over a 300 Hz drone, searching the whole spectrum reported 593 pulses, one
#' for each half cycle of the drone, where the cricket range reported the 5 that
#' are there.
#'
#' Three of the method's parameters have no value anywhere in the paper, and
#' their defaults here are this package's rather than Dietrich's: `psi`, which
#' is named only in the prose and in Algorithm 1; `delta`, described only as
#' making a highly selective filter; and `dt`, the step of Algorithm 1. The one
#' candidate for `dt` is a row of Table 1 reading `beta = U/4` among the
#' segmentation parameters, under a symbol Algorithm 1 does not use and which is
#' the sonogram overlap everywhere else in the paper, so `U/4` is offered rather
#' than assumed and the default is one sample.
#'
#' Onsets and offsets follow the convention described under [pulseDetection()],
#' and the windowed energy displaces each boundary outward in the way described
#' under [pd_simple()]. The one point on which this function parts company with
#' that convention, and with [pd_simple()] and `pd_threshold()`, is a sample
#' whose energy is exactly equal to a threshold. The paper's comparisons are not
#' strict, so such a sample is part of a pulse, and a silent or constant
#' amplitude wave, whose every energy is exactly equal to both thresholds, is
#' reported as one pulse spanning the analysed range. The other two detectors
#' report no pulse in it.
#'
#' @param wave A Wave object
#' @param U Length in samples of the short window the pulse energy is computed
#'   over.
#' @param gamma Where `theta` sits between the smallest and the largest pulse
#'   energy, between 0 and 1.
#' @param alpha Multiplier giving the upper threshold `F_a` from the lower
#'   threshold `F_1`. Values above one put `F_a` above `F_1`.
#' @param scaling Divisor applied to the long window energy before it is added
#'   to `theta`.
#' @param V Length in samples of the long window the reference level is computed
#'   over. Expected to be longer than `U`.
#' @param psi Number of samples after the `F_1` crossing within which the energy
#'   must also cross `F_a` for a pulse to be reported. The paper names this
#'   parameter but never gives it a value.
#' @param dt The step the state machine takes through the wave, in samples. A
#'   boundary can only be reported on this grid. The paper names this parameter
#'   but never gives it a value either; see the details.
#' @param filter Whether to normalise and bandpass filter the wave first, which
#'   is the first step of the method. `FALSE` computes the energy from the wave
#'   as supplied.
#' @param delta Half the width of the passband, in Hz. The paper gives no value
#'   for this, only that the filter is a highly selective one.
#' @param band The frequency range the weighting `W` of equation 2 is defined
#'   over, as a pair in Hz, or `NULL` to search the whole spectrum for the
#'   loudest frequency. The default is the frequency range of cricket song,
#'   which section 1 of the paper gives as 2 to 11 kHz and which is what the
#'   paper's own `W` is a density over. A wave of some other animal needs its
#'   range given here.
#' @export
#' @return A list of input values plus the onset and offset times of pulses
#'
pd_dietrich2004 <- function(
  wave,
  U=120,
  gamma=0.05,
  alpha=1.4,
  scaling=32,
  V=480,
  psi=1,
  dt=1,
  filter=TRUE,
  delta=500,
  band=c(2000, 11000)
){
  #Section 2 of the paper is two steps, and this is the first: the wave is
  #normalised and bandpass filtered, and everything below is computed from the
  #filtered signal rather than from the wave as supplied. The energies and
  #thresholds returned are then in the units of the normalised signal.
  #The step indexes the samples, so a fraction of one would be truncated away
  #sample by sample and give a grid nothing asked for.
  if (length(dt) != 1 || is.na(dt) || dt < 1 || dt != trunc(dt)) {
    stop("dt is the step of Algorithm 1 in samples, so it must be a whole number of samples, one or more.")
  }
  if (filter) {
    wave <- .pd_dietrich_filter(wave, delta=delta, band=band)
  }
  e_u <- ste(wave, U=U)
  e_v <- ste(wave, U=V)

  n <- length(wave)
  #Only the samples both windows cover can be thresholded: ste() leaves a zero
  #where its window does not fit, and both e_u and e_v are read below. e_v was
  #zero across the half of the long window at each end, which collapsed F_1 and
  #F_a to theta there, and theta itself was taken from a floor those zeroes held
  #at zero however loud the recording was.
  t <- .steDefined(n, max(U, V))
  if (length(t) == 0) {
    theta <- NA_real_
  } else {
    energy <- e_u[t]
    theta <- min(energy) + gamma*(max(energy) - min(energy))
  }
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  #Nothing outside the analysed samples is a pulse, as the energy there is not a
  #windowed energy of this wave, so pulse_simple marks the same samples as
  #pd_simple()'s pulse does.
  pulse_simple <- vector(mode="logical", length=n)
  pulse_simple[t] <- e_u[t] >= F_1[t]

  if (length(t) == 0) {
    onsets <- integer(0)
    offsets <- integer(0)
  } else {
    #Algorithm 1 steps through the wave by dt rather than a sample at a time, so
    #the machine only ever sees, and can only ever report, every dt-th sample.
    #The paper never gives dt a value: the one candidate is the row of Table 1
    #reading beta = U/4, under a symbol Algorithm 1 does not use and which is
    #the sonogram overlap everywhere else in the paper, so U/4 is offered rather
    #than assumed.
    steps <- t[seq.int(1, length(t), by=dt)]
    #A pulse already in progress at the first analysed sample is reported with
    #its onset there, as pd_threshold() reports one in progress at the first
    #sample of the wave. Suppressing it while the scan for offsets still reported
    #its end left one more offset than onset, so every later pulse was paired
    #with the previous pulse's offset and pulseStats() gave durations of -2505
    #and -1505 samples for three pulses of 400. A wave wholly above the threshold
    #reported neither an onset nor an offset.
    #The paper's comparison is not strict, so an energy exactly equal to a
    #threshold is part of a pulse. Every energy of a silent or constant wave is
    #exactly equal to both thresholds, so such a wave is reported as one pulse
    #spanning the whole of the analysed range, which is what Algorithm 1 does
    #with it and is where this detector parts company with pd_simple() and
    #pd_threshold().
    gate <- e_u[steps] >= F_1[steps]
    confirm <- e_u[steps] >= F_a[steps]
    onsets <- steps[.pd_dietrich_scan(gate, confirm, psi, dt=dt, armed=TRUE)]
    #The paper gives the state machine for onsets alone and says only that a
    #similar algorithm estimates the offsets. An offset is an onset of the time
    #reversed wave, so the same scan runs backwards over the same two
    #thresholds: both boundaries are then measured at F_1 and a pulse length is
    #its width at F_1. Scanning forwards for a run below F_1 instead cannot work
    #here, because F_a is above F_1 and so an energy below F_1 is below F_a as
    #well: the confirming test would hold at every sample of the run, leaving
    #alpha and psi with no effect at all on where a pulse ends.
    #The reversed scan starts disarmed, so a run still above F_1 at the last
    #analysed sample reports no offset, as the convention requires. A boundary it
    #reports is the last sample of the pulse the machine saw, and the offset is
    #the next sample it would have seen, which is a step later and is inside the
    #analysed range because the run ended before it.
    ends <- .pd_dietrich_scan(rev(gate), rev(confirm), psi, dt=dt, armed=FALSE)
    offsets <- sort(rev(steps)[ends] + as.integer(dt))
    #The two scans are independent of one another, so they can still disagree
    #about how many pulses the wave holds: a run that crosses F_1 without ever
    #reaching F_a gives no onset while the silence after it gives an offset, and
    #a dip below F_1 too short for the counter to test gives a second onset
    #without an offset between the two. Pairing each onset with the offset that
    #closes it keeps the two lists describing the same pulses.
    paired <- .pulsePairs(onsets, offsets)
    onsets <- paired$onsets
    offsets <- paired$offsets
  }

  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    onsets = onsets,
    offsets = offsets,
    pulse_simple = pulse_simple
  ))
}

#' Normalise and bandpass filter a wave, as Dietrich (2004) section 2 does
#'
#' The first step of the method, equations 1 to 4. The wave is divided by its
#' maximum norm, the frequency carrying the most energy is found, and everything
#' further than delta from that frequency is removed, which is what frees the
#' short term energy of the sounds of the environment before it is thresholded.
#'
#' The weighting W of equation 2 is a probability density over the frequencies
#' the sound can occupy. A band gives its support and weights everything inside
#' alike, which is what a density over the frequency range of one group of
#' animals amounts to for the purpose of finding a maximum.
#'
#' @param wave A Wave object.
#' @param delta Half the width of the passband, in Hz.
#' @param band The frequency range W is defined over, as a pair in Hz, or NULL
#'   to search the whole spectrum.
#' @return The filtered wave.
#' @noRd
.pd_dietrich_filter <- function(wave, delta, band=NULL) {
  x <- as.numeric(wave@left)
  n <- length(x)
  #Whether the wave can carry the band at all is a question about the wave and
  #the band, not about what the wave holds, so it is settled before anything
  #that might answer early.
  if (!is.null(band)) {
    nyquist <- wave@samp.rate / 2
    #A band that begins at or above the highest frequency the wave can carry
    #leaves the search nothing to find but the edge of the spectrum, and the
    #passband placed there removes the whole signal. The wave cannot hold the
    #sound the band is for, so say so rather than returning silence.
    if (band[1] >= nyquist) {
      stop(
        "pd_dietrich2004() was asked for a passband in ", band[1], " to ", band[2],
        " Hz, but the wave only reaches ", nyquist,
        " Hz. Pass band to search a range the wave covers, band=NULL for the",
        " whole spectrum, or filter=FALSE to leave the wave unfiltered."
      )
    }
  }
  if (n == 0) {
    return(wave)
  }
  #A sample that cannot be compared carries through the transform into every
  #other sample, so there is no filtered signal to be had. The energy detectors
  #report nothing for such a wave rather than refusing it, as pd_threshold()
  #does, so the unknown signal is passed on as unknown and the thresholds below
  #come out undefined.
  if (anyNA(x)) {
    x[] <- NA_real_
    return(tuneR::Wave(left=x, samp.rate=wave@samp.rate, bit=32, pcm=FALSE))
  }
  #Equation 1. A silent wave has a maximum norm of zero and nothing to
  #normalise, and filtering silence gives silence, so it is returned as it is.
  peak <- max(abs(x))
  if (peak == 0) {
    return(wave)
  }
  x <- x / peak

  spec <- stats::fft(x)
  #The frequency each bin stands for, folded onto the positive axis so a bin and
  #the negative one it mirrors are treated alike, as the |omega| of equation 3
  #treats them.
  bin <- seq_len(n) - 1
  f <- pmin(bin, n - bin) * wave@samp.rate / n

  #Equation 2, the frequency of greatest weighted magnitude.
  magnitude <- Mod(spec)
  if (!is.null(band)) {
    inBand <- f >= band[1] & f <= band[2]
    #A band narrower than the spacing of the bins, or one given the wrong way
    #round, leaves nothing to choose from, and the maximum of a vector that is
    #everywhere -Inf is its first element: the passband would be placed at zero
    #frequency without a word.
    if (!any(inBand)) {
      stop(
        "No frequency of the wave lies between ", band[1], " and ", band[2],
        " Hz, so pd_dietrich2004() cannot place its passband. The wave is",
        " resolved to ", signif(wave@samp.rate/n, 3), " Hz."
      )
    }
    magnitude[!inBand] <- -Inf
  }
  omega <- f[which.max(magnitude)]

  #Equations 3 and 4. The passband is symmetric about zero, so the inverse
  #transform is real bar the rounding of the transform itself.
  spec[abs(f - omega) >= delta] <- 0
  filtered <- Re(stats::fft(spec, inverse=TRUE)) / n

  #The filtered signal is normalised rather than in the units of the wave, so it
  #is held as a floating point Wave whatever the wave it came from was.
  tuneR::Wave(
    left = filtered,
    samp.rate = wave@samp.rate,
    bit = 32,
    pcm = FALSE
  )
}

#' Scan for pulse boundaries using the Dietrich (2004) state machine
#'
#' The state machine in Dietrich (2004) walks the wave one sample at a time, but it
#' only ever reacts to runs of consecutive samples on the same side of the F_1
#' threshold, so it can be evaluated a run at a time instead.
#'
#' Within a run the machine is only armed once a sample on the other side of F_1 has
#' been seen, so the first run is skipped when it starts at the first analysed
#' sample, unless the machine is armed to begin with. The counter tau is then reset
#' every floor(psi)+2 samples, and the sample it is reset on is the candidate
#' boundary OS, which is why F_a is not tested there: the machine spends that sample
#' opening the candidate.
#'
#' The boundary reported for a run is the OS of the cycle in which F_a was first
#' met, not the sample where it was met. The two differ by however long the energy
#' took to climb from F_1 to F_a, which is what psi bounds, so reporting the F_a
#' crossing instead left psi with no effect on the result.
#'
#' The machine disarms as soon as it fires, so each run yields at most one boundary.
#'
#' @param b Logical vector, TRUE where the energy is on the pulse side of F_1.
#' @param a Logical vector, TRUE where the energy is on the pulse side of F_a.
#' @param psi Number of samples a candidate may wait for F_a.
#' @param dt The step the machine takes, in samples. b and a hold one entry per
#'   step, so tau advances by dt per entry and psi is still in samples.
#' @param armed Whether the machine starts armed, so that a run already in
#'   progress at the first analysed sample can yield a boundary.
#' @return A vector of positions within b at which a boundary is detected.
#' @noRd
.pd_dietrich_scan <- function(b, a, psi, dt=1, armed=FALSE) {
  runs <- rle(b)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  #tau reaches psi after floor(psi/dt) steps and is reset on the step after
  #that, so a cycle is floor(psi/dt)+2 steps: one to open the candidate and the
  #rest to wait for F_a.
  cycle <- max(2, floor(psi/dt) + 2)

  if (armed) {
    candidates <- which(runs$values)
  } else {
    candidates <- which(runs$values & starts > 1)
  }
  hits <- vector(mode="integer", length=length(candidates))
  found <- 0
  for (i in candidates) {
    k <- seq_len(runs$lengths[i])
    testable <- k %% cycle != 1
    hit <- which(testable & a[starts[i] + k - 1])
    if (length(hit) > 0) {
      found <- found + 1
      #The OS of the cycle the hit falls in, which is the sample tau was last
      #reset on at or before it.
      hits[found] <- starts[i] + cycle * ((hit[1] - 1) %/% cycle)
    }
  }
  return(hits[seq_len(found)])
}
