o_distance <- function(o1, o2) {
  s <- (o2@position[2] - o1@position[2])^2 + (o2@position[1] - o1@position[1])^2
  return(sqrt(s))
}

o_angle <- function(o1, o2) {
  r <- atan2(o2@position[2]-o1@position[2], o2@position[1]-o1@position[1])
  return(r)
}
