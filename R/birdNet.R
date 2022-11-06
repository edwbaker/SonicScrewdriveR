#' Read output files from BirdNet Analyser
#'
#' Reads a single file, or directory of files, output by BirdNet Analyser.
#'
#' @param file Filename or directory
#' @param filename_parsing Allows for filename parsing, accepted values are one of none, audiomoth.
#' @export
#' @return A data frame.
readBirdNet <- function(file, filename_parsing="none") {
  if (file.exists(file) && !dir.exists(file)) {
    ret <- read.csv(file, sep='\t')
    ret$`Begin.Time..s.` <- as.numeric(ret$`Begin.Time..s.`)
    ret$`End.Time..s.` <- as.numeric(ret$`End.Time..s.`)
    if (filename_parsing == "audiomoth") {
      parts <- strsplit(file, '/')
      filename <- parts[[1]][[length(parts[[1]])]]

      fn_parts <- strsplit(filename, "_")[[1]]
      start <- as.POSIXct(paste(fn_parts[[1]], fn_parts[[2]]), format="%Y%m%d %H%M%OS")

      starts <- start + ret$`Begin.Time..s.`
      ends <- start + ret$`End.Time..s.`
      cn <-colnames(ret)
      ret <- cbind(ret, starts, ends)
      colnames(ret) <- c(cn, "Start", "End")
    }
  } else {
    l <- list.files("~/mac22BirdNet", full.names=T)
    ret <- do.call(rbind, lapply(l, readBirdNet, filename_parsing=filename_parsing))
  }
  return(ret)
}
