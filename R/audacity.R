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

  # ToDo: Stuff above here can use seewave::read.audacity() (which supports also
  # outputs with frequencies) once reported bug of dropping first character is
  # fixed.

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

#' Write an Audacity label file
#'
#' Writes a list of `Annotation` objects to an Audacity label file.
#' \cr\cr
#' Internally this uses the `write.audacity()` function from the `seewave`
#' package \insertCite{seewave2008}{sonicscrewdriver}.
#'
#' @param annotations A list of `Annotation` objects.
#' @param file Path to the Audacity label file.
#' @references
#'   \insertAllCited{}
#' @export
writeAudacityLabels <- function(annotations, file) {
  if (!all(sapply(annotations, inherits, "Annotation"))) {
    stop("Input must be a list of Annotation objects.")
  }

  from=sapply(annotations, function(x) x@start)
  to=sapply(annotations, function(x) x@end)
  label=sapply(annotations, function(x) x@value)
  low <- sapply(annotations, function(x) x@low)
  high <- sapply(annotations, function(x) x@high)

  if(all(low == 0) | !all(high == Inf)) {
    labels <- cbind(label, from, to)
  } else {
    labels <- cbind(label, from, to, low, high)
  }
  seewave::write.audacity(as.data.frame(labels), file)
}
