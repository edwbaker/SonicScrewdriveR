setClass(
  "arena",
  representation(
    max.coord="numeric",
    max.time="numeric",
    members="list",
    overshoot="character",

    #Historical data
    t="numeric",
    centroid="matrix",
    bounding_square="numeric"
  )
)

#' @importFrom methods new
arena <- function(max.coord=10,
                  max.time=10,
                  members=list(NULL),
                  overshoot="allow") {
  a <- new(Class="arena")
  a@max.coord <- max.coord
  a@max.time <- max.time
  a@members <- members
  a@overshoot <- overshoot
  a@centroid <- matrix(nrow=max.time, ncol=2)
  a@bounding_square <- vector(mode="numeric", length=a@max.time)
  return(a)
}

arena.run <- function(arena) {
  arena@members <- lapply(arena@members, arena.init.organism, arena@max.time)
  for (t in 1:arena@max.time) {
    arena@t <- t
    arena@members <- lapply(arena@members, arena.run.organism, arena@members, t, arena)
    bs <- a_bounding_square(arena)
    arena@bounding_square[t] <- bs$bounding_square
    arena@centroid[t,] <- bs$centroid
    if (arena.check.terminate(arena@members)) {break}
  }
  arena <- arena.finish(arena)
  return(arena)
}

arena.run.organism <- function(o, current_state, t, arena) {
  o <- o@strategy(current_state, o, t)
  o@x[t] <- o@position[1]
  o@y[t] <- o@position[2]
  o@d[t] <- o@direction
  return(o)
}

arena.init.organism <- function(o, max.time) {
  o@x <- o@y <- o@d <- vector(mode="numeric", length=max.time)
  o@do_terminate_run <- FALSE
  return(o)
}

arena.validate.position <- function(o, arena) {
  #TODO: wrap, reflect
  return(o)
}

arena.check.terminate <- function(state) {
  for (i in 1:length(state)) {
    if (slot(state[[i]], "do_terminate_run") == TRUE) {
      return(TRUE)
    }
  }
  return(FALSE)
}

arena.finish <- function(arena) {
  for (i in 1:length(arena@members)) {
    arena@members[[i]]@x <- arena@members[[i]]@x[1:arena@t]
    arena@members[[i]]@y <- arena@members[[i]]@y[1:arena@t]

    arena@members[[i]]@path_length <- sum(
      sqrt(diff(arena@members[[i]]@x)^2 + diff(arena@members[[i]]@y)^2))
  }
  a@bounding_square <- a@bounding_square[1:arena@t]
  return(arena)
}

a_extract <- function(arena, s) {
  sapply(arena@members, a_extract_s, s)
}

#' @importFrom methods slot .hasSlot
a_extract_s <- function(member, s) {
  if (.hasSlot(member, s)) {
    return(slot(member,s))
  }
  return(NULL)
}
