#' Various measurements of frequency values for a Wave object
#'
#' Calculates the peak, centre, bandwidth and quality factor. The quality factor (Q) is calculated at both
#' -3dB and -10dB as discussed by Bennett-Clark (1999)  <doi: 10.1080/09524622.1999.9753408>.
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which returns one set of statistics per channel.
#' @param wave_spec A precomputed spectrum (optional, if not present will be
#'   generated), with its frequencies in kHz as seewave::meanspec() returns them.
#' @param plot If TRUE plots the spectrum, with its frequency axis in
#'   output.unit, and marks the edges of the regions measured.
#' @param warn If TRUE provides warnings when values are not consistent
#' @param lowcut Frequency below which values are ignored, in Hz, as every other
#'   frequency the package takes is. This argument was in kHz in earlier
#'   releases: the default is the same frequency as before, but a value given
#'   explicitly needs multiplying by a thousand.
#' @param output.unit Unit of the frequencies and bandwidths returned, "Hz" or
#'   "kHz". The default is "kHz", the unit they have always been returned in,
#'   and will become "Hz", the unit of every other frequency the package takes
#'   or returns, in a future release. A call that leaves it unset warns of this,
#'   once per session; one that sets it, to either unit, gets the same results
#'   before and after that change.
#' @importFrom graphics abline plot
#' @importFrom seewave sfm
#' @return A list with an entry for each of the -3dB and -10dB thresholds, and
#'   the spectral flatness of the spectrum. Each threshold holds the frequencies
#'   `min`, `max`, `longest_min`, `longest_max`, `peak`, `centre`, `outer min`
#'   and `outer max` and the bandwidths `bandwidth` and `outer bandwidth`, all
#'   in output.unit, and the quality factor `Q`. Q and the spectral flatness are
#'   ratios, the same in either unit.
#' @export
#'
frequencyStats <- function(
  wave,
  wave_spec = NULL,
  warn = TRUE,
  lowcut=1000,
  plot=FALSE,
  output.unit="kHz"
) {
  #A vector of units, or none at all, passes a check made of each value in turn,
  #and a factor is looked up in the table of units by its position rather than
  #by its name.
  if (!is.character(output.unit) || length(output.unit) != 1) {
    stop("output.unit must be a single unit, \"Hz\" or \"kHz\".")
  }
  .validateChoice(output.unit, names(.frequencyMultipliers()), "output.unit", "frequencyStats", prep="for")
  if (missing(output.unit)) {
    .warnFrequencyStatsUnit()
  }
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels(). The
    #unit is passed on as resolved here, so that the warning is not raised again
    #for each channel.
    return(allChannels(
      wave,
      function(w) frequencyStats(
        w, wave_spec=wave_spec, warn=warn, lowcut=lowcut, plot=plot, output.unit=output.unit
      ),
      channel.param = NULL
    ))
  }
  validateIsWave(wave)
  if (is.null(wave_spec)) {
    wave_spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  }

  #The spectrum has its frequency axis in kHz, as seewave gives it, and is
  #measured in that unit. What is measured is put in output.unit at the end, so
  #a result in kHz is the spectrum's own value, untouched by any conversion.
  x <- wave_spec[,1]
  y <- wave_spec[,2]

  #lowcut, like every other frequency the package takes, is in Hz.
  y[x < .convertLinear(lowcut, "Hz", "kHz", .frequencyMultipliers())] <- 0

  y<- y^2
  #With nothing above the low cut every threshold is zero, no bin exceeds it, and
  #the outer statistics are taken over an empty set, which gives an infinite
  #frequency and an error about the sample rate rather than about the spectrum.
  if (max(y) == 0) {
    stop("No signal above lowcut, so frequency statistics cannot be calculated.")
  }
  #The results are checked against the sample rate, which is put in kHz with
  #them. It was compared in Hz, a thousand times higher than any frequency the
  #spectrum could hold, so the check could never fail.
  samp.rate <- .convertLinear(wave@samp.rate, "Hz", "kHz", .frequencyMultipliers())
  stats_3 <- .frequencyThresholdStats(x, y, 0.5, samp.rate)
  stats_10 <- .frequencyThresholdStats(x, y, 0.1, samp.rate)

  if(warn) {
    if (stats_3$min >= stats_3$max) {
      warning("-3dB: calculated min is greater than or equal to max, so the region is a single bin")
    }
    if (stats_10$min >= stats_10$max) {
      warning("-10dB: calculated min is greater than or equal to max, so the region is a single bin")
    }
  }

  stats_3 <- .frequencyStatsIn(stats_3, output.unit)
  stats_10 <- .frequencyStatsIn(stats_10, output.unit)

  data <- list(
    "-3dB" = stats_3,
    "-10dB" = stats_10,
    "spectral flatness" = sfm(wave_spec)
  )

  if(plot) {
    #The labels are given to plot() rather than added by title() afterwards,
    #which wrote them over the "x" and "y" that plot() had already drawn.
    plot(
      .convertLinear(x, "kHz", output.unit, .frequencyMultipliers()), y, type="l",
      main="Bandwidth calculations", xlab=paste0("Frequency (", output.unit, ")"), ylab="Amplitude"
    )
    abline(h=0.5*max(y), v=c(stats_3$min, stats_3$max), col="blue")
    abline(h=0.1*max(y), v=c(stats_10$min, stats_10$max), col="green")
    abline(v=c(stats_3$"outer max", stats_3$"outer min"), col="red")
    abline(v=c(stats_10$"outer max", stats_10$"outer min"), col="purple")
  }

  return(data)
}

#' Whether frequencyStats() has warned of its change of unit this session
#'
#' @noRd
.frequencyStatsState <- new.env(parent=emptyenv())

#' Warn that the default unit of frequencyStats() is changing
#'
#' The returned frequencies are changing from kHz to Hz, which a caller cannot
#' see happen: code that worked goes on working, and reads every frequency a
#' thousand times too low. Asking for a unit is how a caller is kept safe across
#' the change, so a call that does not is told so. Once a session is enough to
#' be seen without burying everything else a loop over many recordings warns of.
#'
#' @return No return value, called for its side effect of raising a warning.
#' @noRd
.warnFrequencyStatsUnit <- function() {
  if (isTRUE(.frequencyStatsState$warned)) {
    return(invisible(NULL))
  }
  .frequencyStatsState$warned <- TRUE
  warning(
    "frequencyStats() returns frequencies in kHz when output.unit is not set, and ",
    "will return them in Hz by default in a future release. Set output.unit to ",
    "\"kHz\" to keep these results, or to \"Hz\" for the unit of every other ",
    "frequency the package returns. This warning is shown once per session.",
    call.=FALSE
  )
  return(invisible(NULL))
}

#' Put the frequency statistics at one threshold in the unit asked for
#'
#' @param stats A list as returned by `.frequencyThresholdStats()`, in kHz.
#' @param output.unit "Hz" or "kHz".
#' @return The list with every frequency and bandwidth in output.unit. Q is a
#'   ratio and is left as it is.
#' @noRd
.frequencyStatsIn <- function(stats, output.unit) {
  frequencies <- names(stats) != "Q"
  stats[frequencies] <- lapply(
    stats[frequencies], .convertLinear, from="kHz", to=output.unit, table=.frequencyMultipliers()
  )
  return(stats)
}

#' Frequency statistics at one amplitude threshold
#'
#' frequencyStats() reports the same eleven measurements at two thresholds. They
#' were written out twice, with a second set of variable names, which is how the
#' -3dB and -10dB blocks came to name the same quantity peak_r in one and peak_q
#' in the other.
#'
#' @param x Frequencies of the spectrum, in kHz.
#' @param y Squared amplitudes of the spectrum.
#' @param threshold Proportion of the peak amplitude defining the region, 0.5 for
#'   -3dB and 0.1 for -10dB.
#' @param samp.rate Sample rate of the wave, in kHz with the frequencies, used to
#'   check the results are possible.
#' @return A list of eleven frequency statistics, the frequencies and bandwidths
#'   in kHz.
#' @noRd
.frequencyThresholdStats <- function(x, y, threshold, samp.rate) {
  level <- threshold * max(y)
  r <- rle(y >= level)
  l <- c(0, cumsum(r$lengths))

  regions <- r$lengths[which(r$values == TRUE)]
  regions_l <- l[which(r$values == TRUE)]

  #which.max() rather than which(y==max(y)), which returns every tied bin and so
  #gave a vector where one frequency was meant.
  peak.index <- which.max(y)
  peak <- x[peak.index]

  in.peak <- max(which(regions_l < peak.index))
  longest <- which.max(regions)

  #regions_l holds the bin before each region starts, so the first bin within a
  #region is one further on. Reading the region's lower edge without that offset
  #made every bandwidth one bin too wide and every centre half a bin too low.
  min.f <- x[regions_l[in.peak] + 1]
  max.f <- x[regions_l[in.peak] + regions[in.peak]]

  longest.min <- x[regions_l[longest] + 1]
  longest.max <- x[regions_l[longest] + regions[longest]]

  #The outer statistics are taken over every bin above the threshold, not only
  #those in the region containing the peak.
  above <- x[y > level]

  return(list(
    "min" = validateFreqIsPossible(min.f, samp.rate=samp.rate),
    "max" = validateFreqIsPossible(max.f, samp.rate=samp.rate),
    "longest_min" = validateFreqIsPossible(longest.min, samp.rate=samp.rate),
    "longest_max" = validateFreqIsPossible(longest.max, samp.rate=samp.rate),
    "peak" = validateFreqIsPossible(peak, samp.rate=samp.rate),
    "centre" = validateFreqIsPossible(mean(c(min.f, max.f)), samp.rate=samp.rate),
    "bandwidth" = validateBandwidthIsPossible(max.f - min.f, samp.rate=samp.rate),
    "Q" = validateQ(peak / (max.f - min.f)),
    "outer min" = validateFreqIsPossible(min(above), samp.rate=samp.rate),
    "outer max" = validateFreqIsPossible(max(above), samp.rate=samp.rate),
    "outer bandwidth" = validateBandwidthIsPossible(max(above) - min(above), samp.rate=samp.rate)
  ))
}
