setClass(
  "organism",
  representation(
    position="numeric",
    direction="numeric",
    speed="numeric",
    song_interval="numeric",
    mode="character"
  )
)

o_distance <- function(o1, o2) {
  s <- (o2@position[2] - o1@position[2])^2 + (o2@position[1] - o1@position[1])^2
  return(sqrt(s))
}

o_angle <- function(o1, o2) {
  r <- atan2(o2@position[2]-o1@position[2], o2@position[1]-o1@position[1])
  return(r)
}

o_bit_angle <- function(angle, bits=2, seq=FALSE) {
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
  if (flag==TRUE) { ret <- ret * -1}
  return(ret)
}
