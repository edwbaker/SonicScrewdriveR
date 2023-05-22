ste_dietrich2004 <- function(wave, U) {
  e <- vector(mode="numeric", length=length(wave@left))
  for (i in (U/2+1):(length(wave@left)-U/2)) {
    values <- (i-U/2):(i+U/2)
    values <- values[values > 0]
    e[i] <- sum(abs(wave@left[values]))
  }
  return(e)
}