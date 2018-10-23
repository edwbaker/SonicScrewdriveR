pd_threshold <- function(wave, threshold=0.2, pd=FALSE, U=440) {
  #Prepend zeroes at start to allow for detection of pulses at beginning
  mag <- c(rep.int(0,U), wave@left)
  if (pd==TRUE) {
    mag <- mag ^ 2
  } else {
    mag <- abs(mag)
  }
  
  threshold <- threshold* max(mag)
  onsets <- vector(length=length(mag), mode="logical")
  onsets[1:U] <- FALSE
  for (i in (U + 1):length(mag)) {
    if (mag[i] > threshold & mag[i-1] < threshold) {
      previous <- onsets[(i-U):(i-1)]
      if (length(previous[previous==TRUE]) > 0) {
        onsets[i] <- FALSE
      } else {
        onsets[i] <- TRUE
      }
    } else {
      onsets[i] <- FALSE
    }
  }
  return(which(onsets==TRUE))
}


