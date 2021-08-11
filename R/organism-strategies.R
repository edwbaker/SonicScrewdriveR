brownian <- function(state, organism, t) {
  organism@direction <- runif(1)*360
  organism <- o_move(organism)
  return(organism)
}

emitter <- function(state, organism, t) {
  if (t %% (organism@dutycycle + organism@dutycycle_offset) == 0) {
    organism@is_transmitting <- TRUE
  } else {
    organism@is_transmitting <- FALSE
  }
  return(organism)
}

emitterP <- function(state, organism, t) {
  if (runif(1)*organism@dutycycle < 1) {
    organism@is_transmitting <- TRUE
  } else {
    organism@is_transmitting <- FALSE
  }
  return(organism)
}

receiver <- function(state, organism, t) {
  #Find nearest transmitter
  nearest <- o_nearest(organism, state, "is_transmitting")
  if (is.null(nearest$i)) {return(organism)}
  #Move towards nearest
  organism@direction <- o_bit_angle(o_angle(organism, state[[nearest$i]]),organism@directionbits)
  organism <- o_move(organism)
  if (nearest$d < 1) {
    organism@position <- state[[nearest$i]]@position
    organism@do_terminate_run <- TRUE
    }
  return(organism)
}
