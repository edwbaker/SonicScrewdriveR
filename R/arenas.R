arenas.run <- function(arenas) {
  cl <- defaultCluster()
  res <- pbsapply(arenas, arena.run,cl=cl)
  stopCluster(cl)
  return(res)
}

arenas_grid <- function(arena, xlim, ylim, rep=1) {
  arenas <- replicate(length(xlim[1]:xlim[2])*length(ylim[1]:ylim[2]), arena)
  c <- 0
  for (i in min(xlim):max(xlim)) {
    for (j in min(ylim):max(ylim)) {
      c <- c+1
      for (n in 1:length(a@members)) {
        if (arenas[[c]]@members[[n]]@start_pos_grid == TRUE) {
          arenas[[c]]@members[[n]]@position <- c(i,j)
        }
      }
    }
  }
  return(arenas)
}
