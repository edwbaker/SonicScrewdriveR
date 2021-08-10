arena <- function(max.coord=10,
                  max.time=10,
                  members=list(NULL),
                  overshoot="allow") {
  a <- new(Class="arena")
  a@max.coord <- max.coord
  a@max.time <- max.time
  a@members <- members
  a@overshoot <- overshoot
  return(a)
}

arena.run <- function(arena) {
  arena@members <- lapply(arena@members, arena.init.organism, arena@max.time)
  for (t in 1:arena@max.time) {
    arena@t <- t
    current_state <- arena@members
    new_state <- lapply(current_state, arena.run.organism, current_state, t, arena)
    arena@members <- new_state
    if (arena.check.terminate(new_state)) {break}
  }
  arena <- arena.finish(arena)
  return(arena)
}

arena.run.organism <- function(o, current_state, t, arena) {
  o@x[t] <- o@position[1]
  o@y[t] <- o@position[2]
  o@d[t] <- o@direction
  o <- o@strategy(current_state, o, t)
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
  terminate <- FALSE
  for (i in 1:length(state)) {
    if (slot(state[[i]], "do_terminate_run") == TRUE) {
      terminate <- TRUE
    }
  }
  return(terminate)
}

arena.finish <- function(arena) {
  for (i in 1:length(arena@members)) {
    arena@members[[i]]@x <- arena@members[[i]]@x[1:arena@t]
    arena@members[[i]]@y <- arena@members[[i]]@y[1:arena@t]
  }
  return(arena)
}
