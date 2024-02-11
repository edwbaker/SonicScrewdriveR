#' An S4 class to represent a PseudoWave object that is converted to a
#' Wave object when operated on.
#'
#' @slot type Type of PseudoWave (e.g. "noise")
#' @slot subtype Subtype of PseudoWave (e.g. "white" if type is "noise")
#' @slot scale The Wave channels are multiplied by this value
#' @slot offset This value is added to the  Wave channels
#' @slot seed Random seed for reproducible output
#' @slot scale Logical. Whether to use the random seed value.
setClass(
  "PseudoWave",
  slots=list(
    type="character",
    subtype="character",
    scale="numeric",
    offset="numeric",
    seed="numeric",
    use_seed="logical",
    url="character"
  ),
  prototype = list(
    type = NA_character_,
    subtype = NA_character_,
    scale = 1,
    offset = 0,
    seed = 1,
    use_seed = TRUE,
    url = NA_character_
  )
)

#' Create a PseudoWave object
#'
#' This function is used to create a PseudoWave object that can be used to
#' generate a Wave object when operated on.
#'
#' @param type Type of PseudoWave (e.g. "noise")
#' @param subtype Subtype of PseudoWave (e.g. "white" if type is "noise")
#' @param scale The Wave channels are multiplied by this value
#' @param offset This value is added to the  Wave channels
#' @param seed Random seed for reproducible output
#' @param use_seed Logical. Whether to use the random seed value.
#' @param url URL to download audio from
#' @return A PseudoWave object.
#' @export
pseudoWave <- function(
    type=NA_character_,
    subtype=NA_character_,
    scale=1,
    offset=0,
    seed=1,
    use_seed=TRUE,
    url=NA_character_
) {
  if (is.na(type) & is.na(url)) {
    stop("Either type or url must be specified")
  }
  if (is.na(type) & !is.na(url)) {
    type <- "web"
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
      use_seed=use_seed,
      url=url
    )
  )
}

depseduoWave <- function(pw, n, stereo=NULL, samp.rate, bit, pcm) {
  if (pw@type == "noise") {
    if (pw@use_seed) {set.seed(pw@seed)}
    w <- depseudoNoise(pw@subtype, n, stereo, samp.rate, bit, pcm)
  } else if (pw@type == "web") {
    download.file(pw@url, basename(pw@url))
    w <- readAudio(basename(pw@url))
    stereo <- if (w@stereo)  TRUE else FALSE
    unlink(basename(pw@url))
  }
  w@left <- (w@left * pw@scale) + pw@offset
  if (stereo) {
    w@right <- (w@right * pw@scale) + pw@offset
  }
  return(w)
}

#Use non exported function from tuneR
equalWave <- utils::getFromNamespace("equalWave", "tuneR")

#' @importFrom tuneR noise
depseudoNoise <- function(type, n, stereo, samp.rate, bit, pcm) {
  #This wrapper function is here in case alternative noise functions will be added.
  return(noise(kind=type, duration=n, stereo=stereo, samp.rate=samp.rate, bit=bit, pcm=pcm))
}

setMethod("Arith", signature(e1 = "Wave", e2 = "PseudoWave"),
  function(e1, e2){
    e2 <- depseduoWave(e2, n=length(e1@left), stereo=e1@stereo, samp.rate=e1@samp.rate, bit=e1@bit, pcm=e1@pcm)
    equalWave(e1, e2)
    if(length(e1@left) != length(e2@left))
      stop("Waves must be of equal length for Arithmetics")
    e1@left <- callGeneric(e1@left, e2@left)
    if(e1@stereo)
      e1@right <- callGeneric(e1@right, e2@right)
    e1
})

setMethod("Arith", signature(e1 = "PseudoWave", e2 = "Wave"),
  function(e1, e2){
    e1 <- depseduoWave(e1, n=length(e2@left), stereo=e2@stereo, samp.rate=e2@samp.rate, bit=e2@bit, pcm=e2@pcm)
    equalWave(e1, e2)
    if(length(e1@left) != length(e2@left))
      stop("Waves must be of equal length for Arithmetics")
    e1@left <- callGeneric(e1@left, e2@left)
    if(e1@stereo)
      e1@right <- callGeneric(e1@right, e2@right)
    e1
})


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
    e1
})

#' PseudoWave scalar division
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("/", signature(e1 = "PseudoWave", e2 = "numeric"),
          function(e1, e2){
            validObject(e1)
            e1@scale <- e1@scale/e2
            e1
          })

#' PseudoWave scalar addition
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("+", signature(e1 = "PseudoWave", e2 = "numeric"),
          function(e1, e2){
            validObject(e1)
            e1@offset <- e1@offset+e2
            e1
          })

#' PseudoWave scalar subtraction
#'
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
setMethod("-", signature(e1 = "PseudoWave", e2 = "numeric"),
          function(e1, e2){
            validObject(e1)
            e1@offset <- e1@offset-e2
            e1
          })

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



