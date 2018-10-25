exportPeaks <- function(wave,n=100,scale=1) {
  if (typeof(wave) != "S4" | class(wave) != "Wave") {
    stop("exportPeaks expects a Wave object")
  }
  if (!is.numeric(n) | n != as.integer(n)) {
    stop("n must be an integer")
  }
  if (!is.numeric(scale)) {
    stop("scale must be numeric")
  }
  
  n_samples <- as.integer(length(wave@left) / n)
  
  peaks <- unlist(windowing(wave, window.length=n_samples, FUN=meanPeaks))
  
  peaks <- peaks / max(wave@left) * scale
  
  return(unlist(peaks))
}

meanPeaks <- function(start, wave, window.length) {
  remaining <- min(window.length, length(wave) + 1 - start) -1
  w <- abs(wave@left[start:(start+remaining)])
  return(mean(w))
}