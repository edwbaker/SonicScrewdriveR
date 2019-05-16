aaoOutputNames <- function(data) {
  cnames <- names(data[[1]])
  for (i in 1:length(data[[1]])) {
    if (typeof(data[[1]][[i]]) == "list") {
      for (j in 1:length(data[[1]][[i]])) {
        jnames <- names(data[[1]][[i]])
        if (typeof(data[[1]][[i]][[j]]) == "list") {
          for (k in 1:length(data[[1]][[i]][[j]])) {
            row <- c(row, names(data[[1]][[i]][[j]]))
          }
        } else {
          row <- c(row, paste(cnames[[i]],jnames[[j]], sep="."))
        }
      }
    } else {
      row <- c(row, cnames[[i]])
    }
  }
  return(row)
}
