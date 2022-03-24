setClass(
  "PseudoWave",
  slots=list(
    type="character",
    subtype="character",
    scale="numeric",
    offset="numeric",
    seed="numeric",
    use_seed="logical"
  ),
  prototype = list(
    type = NA_character_,
    subtype = NA_character_,
    scale = 1,
    offset = 0,
    seed = 1,
    use_seed = TRUE
  )
)


depseduoWave <- function(pw, n, stereo, samp.rate, bit, pcm) {
  if (pw@type == "noise") {
    if (pw@use_seed) {set.seed(pw@seed)}
    w <- depseudoNoise(pw@subtype, n, stereo, samp.rate, bit, pcm)
  }
  w@left <- (w@left * pw@scale) + pw@offset
  if (stereo) {
    w@right <- (w@right * pw@scale) + pw@offset
  }
  return(w)
}


#' @importFrom tuneR noise
depseudoNoise <- function(type, n, stereo, samp.rate, bit, pcm) {
  #This wrapper function is here in case alternative noise functions will be added.
  return(noise(kind=type, duration=n, stereo=stereo, samp.rate=samp.rate, bit=bit, pcm=pcm))
}

#' @importFrom methods callGeneric validObject
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

#Use non exported function from tuneR
equalWave <- utils::getFromNamespace("equalWave", "tuneR")

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

setMethod("*", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale*e2
    e1
})

setMethod("/", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale/e2
    e1
})

setMethod("+", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@offset <- e1@offset+e2
    e1
})

setMethod("-", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@offset <- e1@offset-e2
    e1
})

setMethod("*", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@scale <- e2@scale*e1
    return(e2)
  }
)

setMethod("/", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@scale <- e2@scale/e1
    return(e2)
  }
)

setMethod("+", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@offset <- e2@offset+e1
    return(e2)
  }
)

setMethod("-", signature(e1 = "PseudoWave", e2 = "missing"),
function(e1){
  validObject(e1)
  e1@scale <- e1@scale*-1
  e1
})
