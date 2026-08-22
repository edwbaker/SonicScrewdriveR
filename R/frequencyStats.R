#' Various measurements of frequency values for a Wave object
#'
#' Calculates the peak, centre, bandwidth and quality factor. The quality factor (Q) is calculated at both
#' -3dB and -10dB as discussed by Bennett-Clark (1999)  <doi: 10.1080/09524622.1999.9753408>.
#'
#' @param wave A Wave object. Stereo Wave and WaveMC objects are passed to
#'   allChannels(), which returns one set of statistics per channel.
#' @param wave_spec A precomputed spectrum (optional, if not present will be generated)
#' @param plot IF TRUE displays values
#' @param warn If TRUE provides warnings when values are not consistent
#' @param lowcut Frequency (in kHz) values below which are ignored.
#' @importFrom graphics abline plot title
#' @importFrom seewave sfm
#' @return A list of frequency statistics, with an entry for each of the -3dB and -10dB thresholds.
#' @export
#'
frequencyStats <- function(
  wave,
  wave_spec = NULL,
  warn = TRUE,
  lowcut=1,
  plot=FALSE
) {
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(
      wave,
      function(w) frequencyStats(w, wave_spec=wave_spec, warn=warn, lowcut=lowcut, plot=plot),
      channel.param = NULL
    ))
  }
  validateIsWave(wave)
  if (is.null(wave_spec)) {
    wave_spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  }

  x <- wave_spec[,1]
  y <- wave_spec[,2]

  #lowcut
  y[x < lowcut] <- 0

  y<- y^2
  #With nothing above the low cut every threshold is zero, no bin exceeds it, and
  #the outer statistics are taken over an empty set, which gives an infinite
  #frequency and an error about the sample rate rather than about the spectrum.
  if (max(y) == 0) {
    stop("No signal above lowcut, so frequency statistics cannot be calculated.")
  }
  stats_3 <- .frequencyThresholdStats(x, y, 0.5, wave@samp.rate)
  stats_10 <- .frequencyThresholdStats(x, y, 0.1, wave@samp.rate)

  if(warn) {
    if (stats_3$min >= stats_3$max) {
      warning("-3dB: calculated min is greater than or equal to max, so the region is a single bin")
    }
    if (stats_10$min >= stats_10$max) {
      warning("-10dB: calculated min is greater than or equal to max, so the region is a single bin")
    }
  }

  data <- list(
    "-3dB" = stats_3,
    "-10dB" = stats_10,
    "spectral flatness" = sfm(wave_spec)
  )

  if(plot) {
    plot(x,y, type="l")
    abline(h=0.5*max(y), v=c(stats_3$min, stats_3$max), col="blue")
    abline(h=0.1*max(y), v=c(stats_10$min, stats_10$max), col="green")
    abline(v=c(stats_3$"outer max", stats_3$"outer min"), col="red")
    abline(v=c(stats_10$"outer max", stats_10$"outer min"), col="purple")
    title(main="Bandwidth calculations", xlab="Frequency (kHz)", ylab="Amplitude")
  }

  return(data)
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
#' @param samp.rate Sample rate of the wave, used to check the results are possible.
#' @return A list of eleven frequency statistics.
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
