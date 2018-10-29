#' Automatic Band Pass
#'
#' Creates an automatic bandpass filter based on the strongest frequency. The width of the bandwidth can be an
#' integer multiple of the bandwidth at either -3dB or -10dB.
#' 
#' @param wave A Wave object
#' @param bw Either -3dB or -10dB. This is calculated by `frequencyStats`
#' @param n.bw The number of bandwidths either side of the centre of the centre to keep
#' @param lowcut High-pass filtering is applied at this frequency before calculating the centre frequency and bandwith
#' 
#' @export
#' 
autoBandPass <- function(
  wave,
  bw="-3dB",
  n.bw=1,
  lowcut=1000
) {
  wave2 <- seewave::ffilter(wave, from=lowcut, output="Wave")
  data <- frequencyStats(wave2)
  rm(wave2)
  if (bw=="-3dB") {
    bw <- data$`-3dB`$bandwidth * 1000
    centre <- data$`-3dB`$centre * 1000
  }
  if (bw=="-10dB") {
    bw <- data$`-10dB`$bandwidth * 1000
    centre <- data$`-10dB`$centre * 1000
  }
  wave <- seewave::ffilter(
    wave,
    from=centre-n.bw*bw,
    to=centre+n.bw*bw,
    output="Wave"
  )
  return(wave)
}