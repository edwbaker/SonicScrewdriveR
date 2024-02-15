#' List of extra slots for TaggedWave and TaggedWaveMC
#'
#' This function is used to provide a list of new slots for use when extending Wave
#' or WaveMC objects to TaggedWave or TaggedWaveMC.
#' @keywords internal
#' @noRd
.tagSlots <- function(){
  return(list(
    metadata="list",
    analyses="list"
  ))
}

#' A S4 class for tagged waves
#'
#' The TaggedWave class extended the Wave class from the tuneR package so
#' that it can include extended metadata and the results of analyses.
#'
#' @slot metadata A list for storing metdata.
#' @slot analyses A list for storing analyses.
setClass(
  "TaggedWave",
  contains="Wave",
  slots=.tagSlots()
)

#' A S4 class for tagged multi-channel waves
#'
#' The TaggedWaveMC class extended the WaveMC class from the tuneR package so
#' that it can include extended metadata and the results of analyses.
#'
#' @slot metadata A list for storing metdata.
#' @slot analyses A list for storing analyses.
setClass(
  "TaggedWaveMC",
  contains="WaveMC",
  slots=.tagSlots()
)

#' Tag a Wave or WaveMC object
#'
#' This function takes a Wave/WaveMC object and returns a corresponding
#' TaggedWave/TaggedWaveMC object.
#' @param w A Wave or WaveMC object.
#' @return A TaggedWave or TaggedWaveMC object.
#' @importFrom methods as
#' @export
tagWave <- function(w) {
  if (is(w, "Wave")) {
    validateIsWave(w)
    return(as(w, "TaggedWave"))
  } else if (is(w, "WaveMC")) {
    validateIsWaveMC(w)
    return(as(w, "TaggedWaveMC"))
  } else if (is(w, "TaggedWave") | is(w, "TaggedWaveMC")) {
    return(w)
  } else {
    stop("Attempting to tag object that is not of type Wave or WaveMC.")
  }
}

#' Untag a TaggedWave or TaggedWaveMC object
#'
#' This function takes a TaggedWave/TaggedWaveMC object and returns a corresponding
#' Wave/WaveMC object.
#' @param w A TaggedWave or TaggedWaveMC object.
#' @return A Wave or WaveMC object.
#' @importFrom methods as
#' @export
#' @examples
#' \dontrun{
#' w <- noise("white")
#' tw <- tagWave(w)
#' w2 <- untagWave(tw)
#' }
untagWave <- function(w) {
  if (is(w, "TaggedWave")) {
    return(as(w, "Wave"))
  } else if (is(w, "TaggedWaveMC")) {
    return(as(w, "WaveMC"))
  } else if (is(w, "Wave") | is(w, "WaveMC")) {
    return(w)
  } else {
    stop("Attempting to untag object that is not of type TaggedWave or TaggedWaveMC.")
  }
}


