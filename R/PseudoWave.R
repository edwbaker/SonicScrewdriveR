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
#' pw <- pseudoWave("file", system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
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

#Whichever side of the operator the PseudoWave is on, the result is the wave it
#met, holding the new samples, so a tagged wave keeps its tags and the
#arithmetic is recorded in its history. With the PseudoWave on the left the
#result used to be the wave the PseudoWave was turned into, which has no tags.
#.Generic carries the package of the generic as an attribute, which is not part
#of the operator's name.
setMethod("Arith", signature(e1 = "Wave", e2 = "PseudoWave"),
  function(e1, e2){
    op <- as.character(.Generic)
    return(.tagApplyOne(
      e1, .pseudoArith, op, NULL, FALSE, operator=op, pseudoWave=e2, pseudoFirst=FALSE
    ))
  }
)

setMethod("Arith", signature(e1 = "PseudoWave", e2 = "Wave"),
  function(e1, e2){
    op <- as.character(.Generic)
    return(.tagApplyOne(
      e2, .pseudoArith, op, NULL, FALSE, operator=op, pseudoWave=e1, pseudoFirst=TRUE
    ))
  }
)

#' Combine a wave with the samples a PseudoWave stands for
#' @param .w The wave.
#' @param operator The arithmetic operator, as `"+"`.
#' @param pseudoWave The PseudoWave.
#' @param pseudoFirst Whether the PseudoWave was written on the left of the
#'   operator, as the order matters to `-`, `/` and `^`.
#' @return `.w`, holding the result.
#' @keywords internal
#' @noRd
.pseudoArith <- function(.w, operator, pseudoWave, pseudoFirst) {
  p <- depseduoWave(
    pseudoWave, n=length(.w@left), stereo=.w@stereo, samp.rate=.w@samp.rate,
    bit=.w@bit, pcm=.w@pcm
  )
  .equalWave(.w, p)
  op <- match.fun(operator)
  combine <- function(a, b) if (pseudoFirst) op(b, a) else op(a, b)
  .w@left <- combine(.w@left, p@left)
  if (.w@stereo) {
    .w@right <- combine(.w@right, p@right)
  }
  return(.w)
}

# Every multiplication below is written with a space either side of the
# asterisk, including the ones inside a*e2 and b*e2 that used not to be. This
# package builds its documentation with markdown, where a pair of asterisks
# with no space after the opening one marks emphasis, so "a*e2) + b*" was read
# as emphasis and roxygenise() wrote "(g * a\emph{e2) + b}e2" into the help
# page. A spaced asterisk cannot open emphasis, so the arithmetic now survives
# being regenerated.

#' PseudoWave scalar manipulation
#'
#' A PseudoWave is applied as (samples * scale) + offset, so multiplying the
#' whole of it by e2 multiplies the offset as well as the scale:
#' ((g * a) + b) * e2 is (g * a * e2) + b * e2. Scaling only the scale left an
#' offset already set to be added after the multiplication rather than taken
#' into it, so the order the operators were written in was discarded.
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
#' @importFrom methods callGeneric validObject
#' @return A PseudoWave object with its scale and its offset multiplied by e2.
setMethod("*", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale*e2
    e1@offset <- e1@offset*e2
    return(e1)
  }
)

#' PseudoWave scalar division
#'
#' Dividing the whole of a PseudoWave by e2 divides the offset as well as the
#' scale, for the reason given for multiplication above.
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
#' @return A PseudoWave object with its scale and its offset divided by e2.
setMethod("/", signature(e1 = "PseudoWave", e2 = "numeric"),
  function(e1, e2){
    validObject(e1)
    e1@scale <- e1@scale/e2
    e1@offset <- e1@offset/e2
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
#' @return A PseudoWave object with its scale and its offset multiplied by e1.
setMethod("*", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@scale <- e2@scale*e1
    e2@offset <- e2@offset*e1
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

#' Numeric subtraction by PseudoWave
#'
#' A PseudoWave is applied as (samples * scale) + offset, so subtracting one
#' from a number negates the scale and takes the offset from the number:
#' e1 - ((g * scale) + offset) is (g * -scale) + (e1 - offset).
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
#' @return A PseudoWave object giving e1 minus the wave e2 stands for.
setMethod("-", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    validObject(e2)
    e2@scale <- -e2@scale
    e2@offset <- e1 - e2@offset
    return(e2)
  }
)

#' Numeric division by PseudoWave
#'
#' A PseudoWave stands for (samples * scale) + offset, and the reciprocal of
#' that is not of the same form, so dividing a number by a PseudoWave cannot be
#' held as a scale and an offset. The method exists to say so, rather than
#' leaving R to report that a PseudoWave is not a numeric argument.
#' @docType methods
#' @param e1 Input 1
#' @param e2 Input 2
#' @return Nothing: this combination is always an error.
setMethod("/", signature(e1 = "numeric", e2 = "PseudoWave"),
  function(e1, e2){
    stop(paste(
      "Cannot divide a number by a PseudoWave: a PseudoWave holds only a scale",
      "and an offset, and the reciprocal of a wave is neither. Depseudowave it",
      "first, or divide by the Wave the PseudoWave is applied to."
    ))
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
