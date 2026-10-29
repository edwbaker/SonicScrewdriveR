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
  #Every field is read as text and without quoting, so that a label is kept as
  #it was written. Read otherwise, a label of NA was a missing value, double
  #quotes were taken out of a label, and labels left empty were each NA.
  lines <- readLines(file, warn=FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    #A file with no labels in it, as writeAudacityLabels() writes of none.
    labels <- data.frame(from=character(0), to=character(0), label=character(0))
  } else {
    labels <- read.csv2(text=lines, header=FALSE, sep='\t', colClasses="character",
                        quote="", na.strings=character(0))
  }
  colnames(labels)[1:3] <- c("from", "to", "label")
  #A decimal comma is read as read.csv2() read it.
  number <- function(x) as.numeric(sub(",", ".", x, fixed=TRUE))

  #Audacity puts the frequency limits of a label on a row of their own, following
  #the label and beginning with a backslash, rather than in further columns of the
  #label's own row. Read as labels those rows gave annotations with no start time.
  continuation <- trimws(as.character(labels$from)) == "\\"
  low <- rep(0, nrow(labels))
  high <- rep(Inf, nrow(labels))
  if (any(continuation)) {
    carries <- which(continuation) - 1
    low[carries] <- number(labels$to[continuation])
    high[carries] <- number(labels$label[continuation])
    low <- low[!continuation]
    high <- high[!continuation]
    labels <- labels[!continuation, , drop=FALSE]
  }

  labels$from <- number(labels$from)
  labels$to <- number(labels$to)
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

  #vapply() rather than sapply(), which gives a list of no annotations, and
  #is.finite() refused that.
  from <- vapply(annotations, function(x) x@start, numeric(1))
  to <- vapply(annotations, function(x) x@end, numeric(1))
  label <- vapply(annotations, function(x) x@value, character(1))
  low <- vapply(annotations, function(x) x@low, numeric(1))
  high <- vapply(annotations, function(x) x@high, numeric(1))

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
