#' Read an Audacity label file
#'
#' Reads an Audacity label file and returns either a list of `Annotation` objects
#' or a data frame.
#' @param file Path to the Audacity label file.
#' @param output One of "annotations" or "data.frame".
#' @importFrom utils read.csv2
#' @export
readAudacityLabels <- function(file, output="annotations") {
  if (!output %in% c("annotations", "data.frame")) {
    stop("Unknown output format.")
  }
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
      ret[[i]] <- annotation(
        start=labels$from[i],
        end=labels$to[i],
        source="readAudacityLabels",
        file=file,
        value=labels$label[i]
      )
    }
    return(ret)
  }
}
