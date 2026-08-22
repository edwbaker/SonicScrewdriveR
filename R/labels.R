#' Pad labels with interval
#'
#' Takes labels from Google Speech API transcript and pads the time by a specified number of seconds.
#'
#' @param t Transcript from Google Speech API
#' @param pad Amount of time (in seconds) to add to start and end
#' @param max_t Optional. The duration of the file, so padding does not exceed length of file.
#' @export
#' @return A modified Google Speech API transcript object
#' @examples
#' \dontrun{
#' labelPadding(t, pad=2, max_t=duration(wave))
#' }
#'
labelPadding <- function(t, pad=0.5, max_t=NULL) {
  if (is.null(t$timings)) {
    stop("At present this function only handles the outpout of Google Speech API")
  }
  t <- gs_preprocess_transcript(t)
  t$timings$startTime <- validateTimeInSeconds(t$timings$startTime - pad, coerceNegative=TRUE, max_t=max_t, coerceMaximum=TRUE)
  t$timings$endTime <- validateTimeInSeconds(t$timings$endTime + pad, coerceNegative=TRUE, max_t=max_t, coerceMaximum=TRUE)
  return(t)
}

#' Combines labels which overlap into single continuous regions
#'
#' Takes labels from Google Speech API transcript and combines overlapping labels.
#'
#' @param t Transcript from Google Speech API
#' @export
#' @return A list containing start and end times of speech containing regions
#' @examples
#' \dontrun{
#' labelReduction(t)
#' }
#'
labelReduction <- function(t) {
  if (is.null(t$timings)) {
    stop("At present this function only handles the outpout of Google Speech API")
  }
  t <- gs_preprocess_transcript(t)
  return(labelReductionExecute(list(starts=t$timings$startTime, ends=t$timings$endTime)))
}

labelReductionExecute <- function(t) {
  starts <- as.numeric(t$starts)
  ends <- as.numeric(t$ends)
  if (length(starts) == 0) {
    return(list(starts=numeric(0), ends=numeric(0)))
  }

  #Sorting by start means a region can only ever overlap the one being built, so a
  #single pass merges them. Comparing every pair previously read the accumulated
  #regions with the index of the input, which ran off the end of the accumulator,
  #and left a region wholly inside another to be added a second time.
  o <- order(starts)
  starts <- starts[o]
  ends <- ends[o]

  keep.starts <- starts[1]
  keep.ends <- ends[1]
  for (i in seq_along(starts)[-1]) {
    last <- length(keep.starts)
    if (starts[i] <= keep.ends[last]) {
      keep.ends[last] <- max(keep.ends[last], ends[i])
    } else {
      keep.starts <- c(keep.starts, starts[i])
      keep.ends <- c(keep.ends, ends[i])
    }
  }
  starts <- keep.starts
  ends <- keep.ends

  result <- list(starts=starts, ends=ends)
  return(result)
}
