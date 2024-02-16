#' An S4 class to represent a PseudoWave object that is converted to a
#' Wave object when operated on.
#'
#' @slot type Type of PseudoWave (e.g. "noise")
#' @slot subtype Subtype of PseudoWave (e.g. "white" if type is "noise")
#' @slot scale The Wave channels are multiplied by this value
#' @slot offset This value is added to the  Wave channels
#' @slot seed Random seed for reproducible output, NA for no seed
#' @slot scale Logical. Whether to use the random seed value
#' @slot params List of additional parameters to pass to generating function
setClass(
  "PseudoWave",
  slots=list(
    type="character",
    subtype="character",
    scale="numeric",
    offset="numeric",
    seed="numeric",
    params="list"
  ),
  prototype = list(
    type = NA_character_,
    subtype = NA_character_,
    scale = 1,
    offset = 0,
    seed = NA_integer_,
    params = list()
  )
)

#' Create a PseudoWave object
#'
#' This function is used to create a PseudoWave object that can be used to
#' generate a Wave object when operated on.
#'
#' @param type Type of PseudoWave (e.g. "noise", "sine")
#' @param subtype Subtype of PseudoWave (e.g. "white" if type is "noise")
#' @param scale The Wave channels are multiplied by this value
#' @param offset This value is added to the  Wave channels
#' @param seed Random seed for reproducible output. NA for no
#' @param params List of additional parameters to pass to generating function
#' @return A PseudoWave object.
#' @importFrom methods new
#' @export
#' @examples
#' pw <- pseudoWave("noise", "white")
#'
#' pw <- pseudoWave("sine", params=list("f0"=440))
#'
pseudoWave <- function(
    type=NA_character_,
    subtype=NA_character_,
    scale=1,
    offset=0,
    seed=1,
    params=list()
) {
  if (is.na(type)) {
    stop("Type must be specified")
  }
  p <-
  return(
    new(
      "PseudoWave",
      type=type,
      subtype=subtype,
      scale=scale,
      offset=offset,
      seed=seed,
      params=params
    )
  )
}

depseduoWave <- function(pw, n, stereo=NULL, samp.rate, bit, pcm) {
  if (pw@type == "noise") {
    if (!is.na(pw@seed)) {set.seed(pw@seed)}
    w <- .depseudoNoise(pw@subtype, n, stereo, samp.rate, bit, pcm)
  }
  if (pw@type == "sine") {
    w <- .depseudoSine(pw@params$f0, n, stereo, samp.rate, bit, pcm)
  }
  w@left <- (w@left * pw@scale) + pw@offset
  if (stereo) {
    w@right <- (w@right * pw@scale) + pw@offset
  }
  return(w)
}

#' @importFrom tuneR noise
.depseudoNoise <- function(type, n, stereo, samp.rate, bit, pcm) {
  #This wrapper function is here in case alternative noise functions will be added.
  return(
    noise(kind=type, duration=n, stereo=stereo, samp.rate=samp.rate, bit=bit, pcm=pcm)
  )
}

.depseudoSine <- function(freq, duration, stereo, samp.rate, bit, pcm) {
  #This wrapper function is here in case alternative sine functions will be added.
  return(
    sine(freq=freq, duration=duration, stereo=stereo, samp.rate=samp.rate, bit=bit, pcm=pcm)
  )
}

setMethod("Arith", signature(e1 = "Wave", e2 = "PseudoWave"),
  function(e1, e2){
    e2 <- depseduoWave(e2, n=length(e1@left), stereo=e1@stereo, samp.rate=e1@samp.rate, bit=e1@bit, pcm=e1@pcm)
    .equalWave(e1, e2)
    e1@left <- callGeneric(e1@left, e2@left)
    if(e1@stereo)
      e1@right <- callGeneric(e1@right, e2@right)
    return(e1)
  }
)

setMethod("Arith", signature(e1 = "PseudoWave", e2 = "Wave"),
  function(e1, e2){
    e1 <- depseduoWave(e1, n=length(e2@left), stereo=e2@stereo, samp.rate=e2@samp.rate, bit=e2@bit, pcm=e2@pcm)
    .equalWave(e1, e2)
    e1@left <- callGeneric(e1@left, e2@left)
    if(e1@stereo)
      e1@right <- callGeneric(e1@right, e2@right)
    return(e1)
  }
)

#' PseudoWave scalar manipulation
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
#' @importFrom methods callGeneric validObject
setMethod("*", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale*e2
    return(e1)
  }
)

#' PseudoWave scalar division
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("/", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale/e2
    return(e1)
  }
)

#' PseudoWave scalar addition
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("+", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@offset <- e1@offset+e2
    return(e1)
  }
)

#' PseudoWave scalar subtraction
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("-", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@offset <- e1@offset-e2
    return(e1)
  }
)

#' Numeric multiplication by PseudoWave
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("*", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@scale <- e2@scale*e1
    return(e2)
  }
)

#' Numeric addition by PseudoWave
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("+", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@offset <- e2@offset+e1
    return(e2)
  }
)
