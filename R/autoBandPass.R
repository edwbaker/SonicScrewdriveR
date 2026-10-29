#' Automatic Band Pass Filter
#'
#' Creates an automatic bandpass filter based on the strongest frequency. The
#' allowed bandwidth can be an integer multiple of the bandwidth at either -3dB
#' or -10dB.
#'
#' Applied to a `TaggedWave` the call is recorded in the `processing` slot of
#' the result, as described in [tagApply()].
#'
#' @param wave A Wave object
#' @param bw Either -3dB or -10dB. This is calculated by \code{frequencyStats}
#' @param n.bw The number of bandwidths either side of the centre of the centre to keep
#' @param lowcut Frequency (in Hz) below which the recording is ignored, which
#'   must be below half the sample rate. High-pass filtering is applied here
#'   before the centre frequency and bandwidth are calculated, and the same
#'   frequency is the floor of that calculation, so no band below it is chosen.
#' @return A band-pass filtered Wave object
#' @examples
#' \dontrun{
#' autoBandPass(sheep)
#' autoBandPass(sheep, bw="-3dB", n.bw=1, lowcut=1000)
#' autoBandPass(sheep, bw="-10dB", n.bw=2, lowcut=0)
#' }
#' @export
#'
autoBandPass <- function(
  wave,
  bw="-3dB",
  n.bw=1,
  lowcut=1000
) {
  validateIsWave(wave)
  #A whole number given as 1 rather than 1L is a double, so the value is checked
  #rather than the storage type, which rejected the default of this function.
  if (!is.numeric(n.bw) || length(n.bw) != 1 || is.na(n.bw) || n.bw != round(n.bw)) {
    stop("n.bw must be an integer.")
  }
  #Fewer than one bandwidth either side of the centre asks for a band of no
  #width, or one whose edges are the wrong way round, which seewave reports as
  #an error about its own arguments rather than about the number given here.
  if (n.bw < 1) {
    stop("n.bw must be at least 1, as a band needs a bandwidth either side of its centre.")
  }
  .validateChoice(bw, c("-3dB", "-10dB"), "bw", "autoBandPass", prep="for")
  #validateFreq() accepts a vector, where one frequency is meant, and an NA
  #reaches it as a comparison that is neither true nor false.
  if (length(lowcut) != 1 || is.na(lowcut)) {
    stop("lowcut must be a single frequency in Hz.")
  }
  validateFreq(lowcut)
  #A high-pass at or above the Nyquist frequency leaves nothing to measure, and
  #seewave reports a band starting there as an error about its own arguments.
  if (lowcut >= wave@samp.rate/2) {
    stop("lowcut must be below half the sample rate, which is where the recording ends.")
  }
  return(.tagApplyOne(
    wave, .autoBandPass, "autoBandPass", NULL, TRUE,
    bw=bw, n.bw=n.bw, lowcut=lowcut
  ))
}

#' Build and apply an automatic band pass filter without recording it
#'
#' An untagged wave is filtered, as the band is measured and kept with seewave,
#' which rejects the tagged classes.
#' @keywords internal
#' @noRd
.autoBandPass <- function(wave, bw, n.bw, lowcut) {
  wave2 <- seewave::ffilter(wave, from=lowcut, output="Wave")
  wave_spec <- seewave::meanspec(wave2, norm=FALSE, plot=FALSE)
  #The low cut was not passed on at all, which left the default of
  #frequencyStats() standing whatever was asked for here, so no band below that
  #default could be chosen however low the high-pass was set. Its warnings about
  #a single bin are raised for both thresholds, only one of which is used here,
  #so the one that bears on the filter is raised below instead.
  data <- frequencyStats(wave2, wave_spec=wave_spec, lowcut=lowcut, warn=FALSE, output.unit="Hz")
  rm(wave2)
  centre <- data[[bw]]$centre
  bandwidth <- data[[bw]]$bandwidth
  #A signal narrow enough to fall in a single bin of the spectrum, such as a
  #pure tone, has no measurable bandwidth at that threshold, and a band from the
  #centre to the centre holds nothing. The width of a bin is used instead, so
  #that the filter keeps the peak rather than failing in seewave. The spectrum
  #has its frequencies in kHz, as seewave gives them.
  if (bandwidth == 0) {
    bandwidth <- .convertLinear(wave_spec[2, 1] - wave_spec[1, 1], "kHz", "Hz", .frequencyMultipliers())
    #The call is left off, as the band is measured here rather than where the
    #caller can see, and naming the inner call says less than nothing.
    warning(
      bw, ": the signal has no measurable bandwidth, as it falls in a single ",
      "bin of the spectrum. A band one bin wide is kept around the centre.",
      call.=FALSE
    )
  }
  #The band is kept as far as the recording goes rather than beyond it. A wide
  #band around a low centre frequency puts the lower edge below zero, which
  #seewave reports as an error about subscripts rather than as a filter.
  wave <- seewave::ffilter(
    wave,
    from=max(0, centre-n.bw*bandwidth),
    to=min(centre+n.bw*bandwidth, wave@samp.rate/2),
    output="Wave"
  )
  return(wave)
}
