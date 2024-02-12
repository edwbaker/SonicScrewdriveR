#' Various measurements of frequency values for a Wave object
#'
#' Calculates the peak, centre, bandwidth and quality factor. The quality factor (Q) is calculated at both
#' -3dB and -10dB as discussed by Bennett-Clark (1999)  <doi: 10.1080/09524622.1999.9753408>.
#'
#' @param wave A Wave object
#' @param wave_spec A precomputed spectrum (optional, if not present will be generated)
#' @param plot IF TRUE displays values
#' @param warn If TRUE provides warnings when values are not consistent
#' @param lowcut Frequency (in kHz) values below which are ignored.
#' @importFrom graphics abline plot title
#' @importFrom seewave sfm
#' @export
#'
frequencyStats <- function(
  wave,
  wave_spec = NULL,
  warn = TRUE,
  lowcut=1,
  plot=FALSE
) {
  validateIsWave(wave)
  if (is.null(wave_spec)) {
    wave_spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  }

  x <- wave_spec[,1]
  y <- wave_spec[,2]

  #lowcut
  y[x < lowcut] <- 0

  y<- y^2
  a <- y >= 0.5*max(y)
  b <- y >= 0.1*max(y)
  r <- rle(a)
  s <- rle(b)
  l <- c(0,cumsum(r$lengths))
  m <- c(0,cumsum(s$lengths))

  regions <- r$lengths[which(r$values == TRUE)]
  regions_l <- l[which(r$values == TRUE)]

  regions_s <- s$lengths[which(s$values == TRUE)]
  regions_l_s <- m[which(s$values == TRUE)]

  peak_r <- max(which(regions_l < which(y==max(y))))
  peak_q <- max(which(regions_l_s < which(y==max(y))))

  longest <- which(regions == max(regions))
  longest_s <- which(regions_s == max(regions_s))

  min_3 <- x[regions_l[peak_r]]
  max_3 <- x[regions_l[peak_r] + regions[peak_r]]

  outer_min_3 <- x[]

  longest_min_3 <- x[regions_l[longest]]
  longest_max_3 <- x[regions_l[longest] + regions[longest]]

  min_10 <- x[regions_l_s[peak_q]]
  max_10 <- x[regions_l_s[peak_q] + regions_s[peak_q]]

  longest_min_10 <- x[regions_l_s[longest_s]]
  longest_max_10 <- x[regions_l_s[longest_s] + regions_s[longest_s]]

  if(warn) {
    if (min_3 >= max_3) {
      warning("-3dB: calculated max greater than or equal to min")
    }
    if (min_10 >= max_10) {
      warning("-10dB: calculated max greater than or equal to min")
    }
  }

  data <- list(
    "-3dB" = list(
      "min" = validateFreqIsPossible(min_3, samp.rate=wave@samp.rate),
      "max" = validateFreqIsPossible(max_3, samp.rate=wave@samp.rate),
      "longest_min" = validateFreqIsPossible(longest_min_3, samp.rate=wave@samp.rate),
      "longest_max" = validateFreqIsPossible(longest_max_3, samp.rate=wave@samp.rate),
      "peak" = validateFreqIsPossible(x[which(y==max(y))], samp.rate=wave@samp.rate),
      "centre" = validateFreqIsPossible(mean(c(min_3,max_3)), samp.rate=wave@samp.rate),
      "bandwidth" = validateBandwidthIsPossible(max_3 - min_3, samp.rate=wave@samp.rate),
      "Q" = validateQ(x[which(y==max(y))] / (max_3 - min_3)),
      "outer min" = validateFreqIsPossible(min(x[y > 0.5*max(y)]), samp.rate=wave@samp.rate),
      "outer max" = validateFreqIsPossible(max(x[y > 0.5*max(y)]), samp.rate=wave@samp.rate),
      "outer bandwidth" = validateBandwidthIsPossible(max(x[y > 0.5*max(y)]) - min(x[y > 0.5*max(y)]), samp.rate=wave@samp.rate)
    ),
    "-10dB" = list(
      "min" = validateFreqIsPossible(min_10, samp.rate=wave@samp.rate),
      "max" = validateFreqIsPossible(max_10, samp.rate=wave@samp.rate),
      "longest_min" = validateFreqIsPossible(longest_min_10, samp.rate=wave@samp.rate),
      "longest_max" = validateFreqIsPossible(longest_max_10, samp.rate=wave@samp.rate),
      "peak" = validateFreqIsPossible(x[which(y==max(y))], samp.rate=wave@samp.rate),
      "centre" = validateFreqIsPossible(mean(c(min_10,max_10)), samp.rate=wave@samp.rate),
      "bandwidth" = validateBandwidthIsPossible(max_10 - min_10, samp.rate=wave@samp.rate),
      "Q" = validateQ(x[which(y==max(y))] / (max_10 - min_10)),
      "outer min" = validateFreqIsPossible(min(x[y > 0.1*max(y)]), samp.rate=wave@samp.rate),
      "outer max" = validateFreqIsPossible(max(x[y > 0.1*max(y)]), samp.rate=wave@samp.rate),
      "outer bandwidth" = validateBandwidthIsPossible(max(x[y > 0.1*max(y)]) - min(x[y > 0.1*max(y)]), samp.rate=wave@samp.rate)
    ),
    "spectral flatness" = sfm(wave_spec)
  )

  if(plot) {
    plot(x,y, type="l")
    abline(h=0.5*max(y), v=c(min_3,max_3), col="blue")
    abline(h=0.1*max(y), v=c(min_10,max_10), col="green")
    abline(v=c(max(x[y > 0.5*max(y)]), min(x[y > 0.5*max(y)])), col="red")
    abline(v=c(max(x[y > 0.1*max(y)]), min(x[y > 0.1*max(y)])), col="purple")
    title(main="Bandwidth calculations", xlab="Frequency (kHz)", ylab="Amplitude")
  }

  return(data)
}
