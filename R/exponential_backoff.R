exponential_backoff <- function(try, tries=10, first=1, last=10, integer=TRUE) {
  if (try > tries) {
    try <- tries
  }
  if (tries == 1) {
    #A single try has no interval to spread over.
    return(if (integer) round(first) else first)
  }
  start <- log(first)
  end <- log(last)
  interval <- (end-start)/(tries-1)
  result <- exp(start + (try-1)*interval)
  if (integer) {
    return(round(result))
  } else {
    return(result)
  }
}