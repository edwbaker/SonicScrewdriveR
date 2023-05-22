#' @importFrom stats runif
brownian <- function(state, organism, t) {
  if (typeof(state)=="character" && state=="info") {
    return(list(
      "name" = "Brownian motion",
      "category" = "stochastic",
      "mobility" = "mobile"
    ))
  }
  organism@direction <- runif(1)*2*pi
  organism <- o_move(organism)
  return(organism)
}

emitter <- function(state, organism, t) {
  if (typeof(state)=="character" && state=="info") {
    return(list(
      "name" = "Basic Emitter",
      "category" = "emitter",
      "mobility" = "static"
    ))
  }
  if (t %% (organism@dutycycle + organism@dutycycle_offset) == 0) {
    organism@is_transmitting <- TRUE
  } else {
    organism@is_transmitting <- FALSE
  }
  return(organism)
}

emitterP <- function(state, organism, t) {
  if (typeof(state)=="character" && state=="info") {
    return(list(
      "name" = "Probabilistic Emitter",
      "category" = "emitter",
      "mobility" = "mobile"
    ))
  }
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

spacerV <- function(state, organism, t) {
  #Find nearest transmitter - if none return
  nearest <- o_nearest(organism, state, "is_transmitting")
  if (is.null(nearest$i)) {return(organism)}
  if (nearest$d > 10) {return(organism)}

  v <- o_mean_vector(organism, state, "is_transmitting")
  organism@direction <- pi + o_bit_angle(v$r, bits=organism@directionbits)
  organism <- o_move(organism)
  return(organism)
}

spacer <- function(state, organism, t) {
  #Find nearest transmitter
  nearest <- o_nearest(organism, state, "is_transmitting")
  if (is.null(nearest$i)) {return(organism)}
  if (nearest$d < 10) {
    organism@direction <- pi + o_bit_angle(o_angle(organism, state[[nearest$i]]),organism@directionbits)
    organism <- o_move(organism)
  }
  return(organism)
}
