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
  starts <- c(t$starts[[1]])
  ends <- c(t$ends[[1]])
  for (i in 2:length(t$starts)) {
    overlap <- FALSE
    for (j in 1:length(starts)) {
      if (t$starts[[i]] >= starts[[j]] & t$starts[[i]] <= ends[[j]]) {
        if (t$ends[[i]] >= ends[[j]]) {
          ends[[j]] <- t$ends[[i]]
          overlap <- TRUE
        }
      } else if (t$starts[[i]] <= starts[[j]] & t$ends[[i]] <= ends[[j]]) {
        starts[[j]] <- starts[[i]]
        overlap <- TRUE
      } else if (t$starts[[i]] <= starts[[j]] & t$ends[[i]] >= ends[[j]]) {
        starts[[j]] <- starts[[i]]
        ends[[j]] <- ends[[i]]
        overlap <- TRUE
      }
    }
    if (overlap==FALSE) {
      starts <- c(starts, t$starts[[i]])
      ends <- c(ends, t$ends[[i]])
    }
  }
  result <- list(starts=starts, ends=ends)
  return(result)
}
