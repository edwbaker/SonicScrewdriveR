organism <- function(position=NULL,
                     direction=0,
                     speed=0,
                     amplitude=0,
                     strategy=brownian,
                     arena=NULL,
                     dutycycle=1,
                     dutycycle_offset=0,
                     directionbits=Inf) {
  o <- new(Class="organism")
  if (is.numeric(position)) {
    o <- o_position(o,arena=arena, x=position[1],y=position[2])
    o@start_pos_random <- FALSE
    o@start_pos_grid <- FALSE
  } else if (position=="random") {
    o@start_pos_random <- TRUE
    o@start_pos_grid <- FALSE
  } else if (position=="grid") {
    o@start_pos_grid <- TRUE
    o@start_pos_random <- FALSE
  }
  o@direction <- direction
  o@speed <- speed
  o@amplitude <- amplitude
  o@strategy <- strategy
  o@dutycycle <- dutycycle
  o@dutycycle_offset <- dutycycle_offset
  o@directionbits <- directionbits

  o@is_transmitting <- FALSE
  return(o)
}

o_position <- function(o, arena=NULL, x, y, mode="absolute") {
  if (mode == "absolute") {
    o@position <- c(x,y)
  }
  if (mode == "random") {
    o@position <- runif(2)*arena@max.coord*2 - arena@max.coord
  }
  return(o)
}
