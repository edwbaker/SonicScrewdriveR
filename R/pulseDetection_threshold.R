pd_threshold <- function(wave, threshold=0.2, pd=FALSE, U=1) {
  #Prepend zeroes at start to allow for detection of pulses at beginning
  mag <- c(rep.int(0,U), wave@left)
  if (pd==TRUE) {
    mag <- mag ^ 2
  } else {
    mag <- abs(mag)
  }

  threshold <- threshold* max(mag)
  n <- length(mag)
  if (n < U + 1) {
    return(list(
      onsets = integer(0),
      offsets = integer(0)
    ))
  }

  # A pulse starts where the magnitude crosses the threshold upwards and ends where
  # it crosses downwards, so both sets of candidates can be found without a loop.
  above <- mag > threshold
  below <- mag < threshold
  onsets <- c(FALSE, above[-1] & below[-n])
  offsets <- c(FALSE, below[-1] & above[-n])
  onsets[seq_len(U)] <- FALSE
  offsets[seq_len(U)] <- FALSE

  #Positions are found in the padded magnitudes, so the padding is removed again to
  #give positions in the wave itself.
  return(list(
    onsets = .pd_threshold_debounce(which(onsets), U) - U,
    offsets = .pd_threshold_debounce(which(offsets), U) - U
    )
  )
}

#' Discard threshold crossings that follow too closely on an accepted one
#'
#' A crossing is only accepted if no crossing has already been accepted within the
#' preceding U samples. Accepted crossings are increasing, so only the most recent
#' one has to be considered.
#'
#' @param crossings A vector of candidate crossing positions, in increasing order.
#' @param U Minimum separation between accepted crossings, in samples.
#' @return A vector of accepted crossing positions.
#' @noRd
.pd_threshold_debounce <- function(crossings, U) {
  accepted <- vector(mode="integer", length=length(crossings))
  found <- 0
  last <- -Inf
  for (i in crossings) {
    if (i - last > U) {
      found <- found + 1
      accepted[found] <- i
      last <- i
    }
  }
  return(accepted[seq_len(found)])
}
