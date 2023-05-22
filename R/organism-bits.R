o_bit_angle <- function(angle, bits=2, seq=FALSE) {
  if (bits==Inf) {return(angle)}
  buckets <- 2^bits
  s <- vector(mode="numeric", length=buckets+1)
  a <- 0
  ai <- 2*pi/buckets
  i <- 1
  s[0] <- 0
  while (a < 2*pi) {
    i <- i + 1
    a <- a+ai
    s[i] <- min(a, 2*pi)
  }
  if (seq==TRUE) {return(s)}
  flag=FALSE
  if (angle < 0) {flag=TRUE}
  ret <- s[which.min(abs(s - abs(angle)))]
  if (flag==TRUE) { return(ret * -1)}
  return(ret)
}

o_bit <- function(n, range=c(0,1), bits=2, model="linear", seq=FALSE) {
  buckets <- 2^bits
  s <- vector(mode="numeric", length=buckets+1)
  a <- 0
  ai <- diff(range)/buckets
  i <- 1
  s[0] <- 0
  while (a < max(range)) {
    i <- i + 1
    a <- a+ai
    s[i] <- min(a, max(range))
  }
  if (seq==TRUE) {return(s)}
  flag=FALSE
  if (n < 0) {flag=TRUE}
  ret <- s[which.min(abs(s - abs(n)))]
  if (flag==TRUE) { return(ret * -1)}
  return(ret)
}
