#' An S4 class to represent a PseudoWave object that is converted to a
#' Wave object when operated on.
#'
#' @slot type Type of PseudoWave (e.g. "noise")
#' @slot subtype Subtype of PseudoWave (e.g. "white" if type is "noise")
#' @slot scale The Wave channels are multiplied by this value
#' @slot offset This value is added to the  Wave channels
#' @slot seed Random seed, so that the same PseudoWave gives the same samples
#'   every time. NA draws a fresh sequence on each use.
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
    seed = 1,
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
#' @param seed Random seed, so that the same PseudoWave gives the same samples
#'   every time. Pass NA for a fresh random sequence on each use. Setting a seed
#'   does not disturb the random numbers drawn elsewhere in the session.
#' @param params List of additional parameters to pass to generating function
#' @return A PseudoWave object.
#' @importFrom methods new
#' @export
#' @examples
#' pw <- pseudoWave("noise", "white")
#'
#' pw <- pseudoWave("sine", params=list("f0"=440))
#'
#' \dontrun{
#' pw <- pseudoWave("file", "myfile.wav")
#' }
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
  if (!type %in% .pseudoWaveTypes()) {
    stop(paste("Unsupported PseudoWave type:", type))
  }
  #A bare NA is logical, and the slot holds a number, so asking for a fresh
  #sequence the obvious way would otherwise be rejected by the class.
  if (length(seed) != 1) {
    stop("A PseudoWave takes a single seed, or NA for a fresh sequence each time.")
  }
  seed <- as.numeric(seed)
  if (type=="file"){
    # params list must have file set
    if (is.na(subtype)) {
      stop("Filename must be specified")
    }
    if (!file.exists(subtype)) {
      stop("File does not exist")
    }
  }
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
  if (!pw@type %in% .pseudoWaveTypes()) {
    stop(paste("Unsupported PseudoWave type:", pw@type))
  }
  if (pw@type == "file") {
    w <- readAudio(pw@subtype)
    stereo <- w@stereo
    #The file holds however many samples it holds, but the wave being operated on
    #decides how many are wanted. Leaving them alone let the arithmetic recycle
    #the file against the target, silently and however badly the lengths matched.
    w@left <- rep_len(w@left, n)
    if (stereo) {
      w@right <- rep_len(w@right, n)
    }
  }
  if (pw@type == "noise") {
    if (!is.na(pw@seed)) {
      .withSeed(pw@seed)
      set.seed(pw@seed)
    }
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
#' @return A PseudoWave object with its scale multiplied by e2.
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
#' @return A PseudoWave object with its scale divided by e2.
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
#' @return A PseudoWave object with e2 added to its offset.
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
#' @return A PseudoWave object with e2 subtracted from its offset.
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
#' @return A PseudoWave object with its scale multiplied by e1.
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
#' @return A PseudoWave object with e1 added to its offset.
setMethod("+", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@offset <- e2@offset+e1
    return(e2)
  }
)

#' Types of PseudoWave that can be generated
#'
#' @return A character vector of the supported types.
#' @noRd
.pseudoWaveTypes <- function() {
  return(c("file", "noise", "sine"))
}

#' Arrange for the caller's random stream to be restored
#'
#' set.seed() replaces the random stream of the whole session, so a PseudoWave
#' asked for reproducible noise used to change every random number drawn after it.
#' Called from the frame that is about to set the seed, this registers an exit
#' handler on that frame which puts the stream back.
#'
#' @param seed The seed about to be set, used only to skip the work when there is
#'   none.
#' @return Called for its side effect.
#' @noRd
.withSeed <- function(seed) {
  frame <- parent.frame()
  if (exists(".Random.seed", envir=globalenv(), inherits=FALSE)) {
    previous <- get(".Random.seed", envir=globalenv(), inherits=FALSE)
    do.call(
      "on.exit",
      list(quote(assign(".Random.seed", previous, envir=globalenv())), add=TRUE),
      envir = frame
    )
    assign("previous", previous, envir=frame)
  } else {
    #There was no stream before, so the one this creates is removed again.
    do.call(
      "on.exit",
      list(quote(rm(".Random.seed", envir=globalenv())), add=TRUE),
      envir = frame
    )
  }
  return(invisible(NULL))
}
