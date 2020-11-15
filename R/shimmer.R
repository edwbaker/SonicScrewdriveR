#' Shimmer calculation
#'
#' Calculation of shimmer (variation of amplitude) between consecutive periods.
#'
#' @param wave The wave object being analysed.
#' @param method One of "db".
#' @param pulses Precalculated pulses from the pulseDetection function, or a list of onset and offset times (in samples).
#' @param pulse_method Shortcut to run pulseDetection from this function.
#'
#' The "dB" method calculates the variation between  the maximum absolute amplitude
#' between successive periods (not the peak-to-peak amplitude).
#'
#' @examples
#' \dontrun{
#' data(sheep)
#' shimmer(sheep, pulse_method="threshold")
#' }
#'
shimmer <- function(wave,
                    method="dB",
                    pulses=NULL,
                    pulse_method=NULL
){
  if (is.null(pulses) && is.null(pulse_method)) {
    stop("pulses or pulse_method is required for shimmer calculation.")
  }
  if (is.null(pulses)) {
    pulses <- pulseDetection(wave, method=pulse_method)
  }

  if (method == "dB") {
    s <- shimmer_db(wave, pulses=pulses)
    return(s)
  }
}

shimmer_db <- function(wave, pulses) {
  n <- length(pulses$onsets)
  A <- vector(mode="numeric", length=n)
  wave@left <- abs(wave@left)
  for (i in 1:n) {
    A[i] <- max(wave@left[pulses$onsets[i]:pulses$offsets[i]])
  }
  ratio <- vector(mode="numeric", length=n-1)
  for (i in 1:(n-1)) {
    ratio[i] <- A[i+1] / A[i]
  }
  dB <- 20 * log10(ratio)
  shimmer <- sum(dB) / (n-1)
  return(shimmer)
}
