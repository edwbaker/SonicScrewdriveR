#' Read an Audacity label file
#'
#' Reads an Audacity label file and returns either a list of `Annotation` objects
#' or a data frame.
#' @param file Path to the Audacity label file.
#' @param output One of "annotations" or "data.frame".
#' @importFrom utils read.csv2
#' @return A list of Annotation objects, or a data frame if output is "data.frame".
#' @export
readAudacityLabels <- function(file, output="annotations") {
  .validateChoice(output, c("annotations", "data.frame"), msg="Unknown output format.")
  labels <- read.csv2(file, header=FALSE, sep='\t')
  colnames(labels)[1:3] <- c("from", "to", "label")

  #Audacity puts the frequency limits of a label on a row of their own, following
  #the label and beginning with a backslash, rather than in further columns of the
  #label's own row. Read as labels those rows gave annotations with no start time.
  continuation <- trimws(as.character(labels$from)) == "\\"
  low <- rep(0, nrow(labels))
  high <- rep(Inf, nrow(labels))
  if (any(continuation)) {
    carries <- which(continuation) - 1
    low[carries] <- as.numeric(labels$to[continuation])
    high[carries] <- as.numeric(labels$label[continuation])
    low <- low[!continuation]
    high <- high[!continuation]
    labels <- labels[!continuation, , drop=FALSE]
  }

  labels$from <- as.numeric(labels$from)
  labels$to <- as.numeric(labels$to)
  labels$low <- low
  labels$high <- high

  # ToDo: Stuff above here can use seewave::read.audacity() (which supports also
  # outputs with frequencies) once reported bug of dropping first character is
  # fixed.

  if (output=="data.frame") {
    return(labels)
  }
  if (output=="annotations") {
    ret <- vector("list", length=nrow(labels))
    for (i in seq_len(nrow(labels))) {
      ret[[i]] <- annotation(
        start=labels$from[i],
        end=labels$to[i],
        low=labels$low[i],
        high=labels$high[i],
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
#' @return No return value, called for its side effect of writing a label file.
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

  #Frequency limits are worth writing when any of them says something. The test
  #used to hold the other way round, so they were written only when they did not.
  informative <- any(low != 0, na.rm=TRUE) || any(is.finite(high), na.rm=TRUE)

  #seewave::write.audacity() reads its five column form by name and its three
  #column form by position, as label, start, end.
  if (informative) {
    labels <- data.frame(t1=from, t2=to, label=label, f1=low, f2=high)
  } else {
    labels <- data.frame(label=label, t1=from, t2=to)
  }
  seewave::write.audacity(labels, file)
}
