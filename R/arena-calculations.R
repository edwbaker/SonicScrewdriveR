a_centroid <- function(arena, time=NULL) {
  x <- a_extract(arena, "x")
  y <- a_extract(arena, "y")
  if (is.null(time)) {
    time <- arena@t
  }
  return(c(mean(x[time,]), mean(y[time,])))
}

a_bounding_square <- function(arena, time=NULL) {
  c <- a_centroid(arena)
  x <- a_extract(arena, "x")
  y <- a_extract(arena, "y")
  if (is.null(time)) {
    time <- arena@t
  }
  return(list(
    "centroid" = c,
    "bounding_square" = max(c(abs(x[time,]),y[time,])) - max(abs(c))
  ))
}
