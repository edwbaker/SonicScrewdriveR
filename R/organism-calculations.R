o_distance <- function(o1, o2) {
  s <- sqrt(sum((o2@position - o1@position)^2))
  return(s)
}

o_angle <- function(o1, o2) {
  r <- atan2(o2@position[2]-o1@position[2], o2@position[1]-o1@position[1])
  return(r)
}

o_nearest <- function(o, state, status="is_transmitting") {
  nearest_value <- Inf
  nearest_i <- NULL
  for (i in 1:length(state)) {
    if (slot(state[[i]], status) == TRUE) {
      d <- o_distance(o, state[[i]])
      if (d != 0 && d < nearest_value) {
        nearest_value <- d
        nearest_i <- i
      }
    }
  }
  return(list(
    "d"=nearest_value,
    "i"=nearest_i
  ))
}

o_mean_vector <- function(o, state, status="is_transmitting") {
  x <- y <- 0
  for (i in 1:length(state)) {
    if (slot(state[[i]], status) == TRUE) {
      d <- o_distance(o, state[[i]])
      if (d==0) {break}
      d <- 1/o_distance(o, state[[i]])
      a <- o_angle(o, state[[i]])
      x <- x + d*cos(a)
      y <- y + d*sin(a)
    }
  }
  r <- atan2(y,x)
  A <- sqrt(x^2+y^2)
  return(list(
    "r"=r,
    "A"=A
  ))
}

o_move <- function(o) {
  o@position[1] <- o@position[1] + o@speed*cos(o@direction)
  o@position[2] <- o@position[2] + o@speed*sin(o@direction)
  return(o)
}
