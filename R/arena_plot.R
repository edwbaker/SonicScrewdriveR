arena.plot <- function(arena, type="basic", t=NULL, ...) {
  if (type=="basic") {
    arena.plot.basic(arena, ...)
  }
  if (type=="movement") {
    arena.plot.movement(arena, ...)
  }
  if (type=="vectorfield") {
    arena.plot.vectorfield(arena, ...)
  }
}

arena.plot.basic <- function(arena, t, ...) {
  #ToDo: plot is too specific
  plot(arena@members[[2]]@x,
       arena@members[[2]]@y,
       type="l",
       xlim=c(-50,10),
       ylim=c(0,10))
}

#' @importFrom graphics arrows points
arena.plot.movement <- function(a) {
  m <- max(a_extract(a, "x"), a_extract(a, "y"))
  plot(0,
       0,
       xlim=c(-m,m),
       ylim=c(-m,m),
       xlab="",
       ylab="",
       asp=1,
       type="n"
  )
  points(a_extract(a, "x")[1,],
         a_extract(a, "y")[1,]
         )
  arrows(a_extract(a, "x")[1,],
         a_extract(a, "y")[1,],
         a_extract(a, "x")[a@max.time,],
         a_extract(a, "y")[a@max.time,],
         angle=10,
         length=.1
  )

  for (i in 1:length(a@members)) {
    lines(a_extract(a, "x")[,i],
          a_extract(a, "y")[,i],
          col="grey"
    )
  }
}

arena.plot.vectorfield <- function(arena, ...){

}
