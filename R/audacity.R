#' @importFrom utils read.csv2
readAudacityLabelFile <- function(file, output="annotations") {
  labels <- read.csv2(file,header=F,sep='\t')
  colnames(labels) <- c("from", "to", "label")

  labels$from <- as.numeric(labels$from)
  labels$to <- as.numeric(labels$to)

  if (output=="data.frame") {
    return(labels)
  }
  if (output=="annotations") {
    ret <- vector("list", length=nrow(labels))
    for (i in 1:nrow(labels)) {
      obj <- list(new("Annotation",
                    start=labels$from[i],
                    end=labels$to[i],
                    source=file,
                    value=labels$label[i]))
      ret[i] <- obj
    }
    return(ret)
  }
}
