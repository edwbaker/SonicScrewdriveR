arena.plot <- function(arena) {
  plot(arena@members[[2]]@x, arena@members[[2]]@y, type="l", xlim=c(-50,10), ylim=c(0,10))
  points(0,0)
  points(10,10)
  print(arena@members[[2]]@x)
  print(length(arena@members[[2]]@x))
}
