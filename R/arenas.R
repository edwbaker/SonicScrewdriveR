arenas.run <- function(arenas, rep=1) {
  cl <- defaultCluster()
  clusterSetRNGStream(cl, iseed=42)
  arenas <- replicate(rep, arenas)
  ret <- pblapply(arenas, arena.run,cl=cl)
  ret <- matrix(ret, ncol=rep)
  stopCluster(cl)
  return(ret)
}


arenas_grid <- function(arena, xlim, ylim) {
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

arenas_directionbits <- function(arena, bits, pos) {
  arenas <- replicate(length(bits), arena)
  for (i in 1:length(arenas)) {
    for (j in pos) {
      arenas[[i]]@members[[j]]@directionbits <- bits[[i]]
    }
  }
  return(arenas)
}

as_extract <- function(arenas, member, s, output=NULL) {
  ret <- lapply(arenas, as_extract_helper, member, s)
  ret <- matrix(unlist(ret), ncol=ncol(arenas))
  if (is.null(output)) {
    return(ret)
  }
  if (output == "mean") {
    return(apply(ret,1,mean))
  }
}

as_extract_helper <- function(arena, member, s) {
  r <- a_extract_s(arena@members[[member]], s)
  return(r)
}

