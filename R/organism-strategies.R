brownian <- function(state, organism, t) {
  organism@direction <- runif(1)*360
  organism <- o_move(organism)
  return(organism)
}

emitter <- function(state, organism, t) {
  organism@is_transmitting <- TRUE
  return(organism)
}

receiver <- function(state, organism, t) {
  #Find nearest transmitter
  nearest <- o_nearest(organism, state, "is_transmitting")
  if (is.null(nearest$i)) {return(organism)}
  #Move towards nearest
  organism@direction <- o_bit_angle(o_angle(organism, state[[nearest$i]]),8)
  organism <- o_move(organism)
  if (nearest$d < 1) {organism@do_terminate_run <- TRUE}
  return(organism)
}
