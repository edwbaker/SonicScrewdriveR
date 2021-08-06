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

o_bit_angle <- function(angle, bits=2) {
  flag=FALSE
  if (angle < 0) {flag=TRUE}
  buckets <- 2^bits
  s <- seq(from=0, to=2*pi, length.out=buckets+1)
  ret <- s[which.min(abs(s - abs(angle)))]
  if (flag==TRUE) { ret <- ret * -1}
  return(ret)
}
