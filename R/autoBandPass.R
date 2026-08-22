#' Automatic Band Pass Filter
#'
#' Creates an automatic bandpass filter based on the strongest frequency. The
#' allowed bandwidth can be an integer multiple of the bandwidth at either -3dB
#' or -10dB.
#'
#' @param wave A Wave object
#' @param bw Either -3dB or -10dB. This is calculated by \code{frequencyStats}
#' @param n.bw The number of bandwidths either side of the centre of the centre to keep
#' @param lowcut High-pass filtering is applied at this frequency before calculating the centre frequency and bandwidth
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
  .validateChoice(bw, c("-3dB", "-10dB"), "bw", "autoBandPass", prep="for")
  wave2 <- seewave::ffilter(wave, from=lowcut, output="Wave")
  data <- frequencyStats(wave2)
  rm(wave2)
  centre <- data[[bw]]$centre * 1000
  bw <- data[[bw]]$bandwidth * 1000
  wave <- seewave::ffilter(
    wave,
    from=centre-n.bw*bw,
    to=centre+n.bw*bw,
    output="Wave"
  )
  return(wave)
}
