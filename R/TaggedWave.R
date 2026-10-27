#' List of extra slots for TaggedWave and TaggedWaveMC
#'
#' This function is used to provide a list of new slots for use when extending Wave
#' or WaveMC objects to TaggedWave or TaggedWaveMC.
#' @keywords internal
#' @noRd
.tagSlots <- function(){
  return(list(
    origin="character",
    metadata="list",
    processing="list",
    spectrogram_maad_power = "spectrogram_maad"
  ))
}

#' A S4 class for tagged waves
#'
#' The TaggedWave class extended the Wave class from the tuneR package so
#' that it can include extended metadata and the results of analyses.
#'
#' @slot origin The origin of the object.
#' @slot metadata A list for storing metadata.
#' @slot processing A list recording, in order, each process applied to the object.
#' @slot spectrogram_maad_power A scikit-maad power spectrogram.
setClass(
  "TaggedWave",
  contains="Wave",
  slots=.tagSlots()
)

#' Build a process record
#'
#' Optional fields are only included when supplied, so a record made with just
#' `process` and `output` keeps its original two-field form.
#' @keywords internal
#' @noRd
.processRecord <- function(process, output=NULL, duration=NULL, func=NULL,
                           params=NULL, time=NULL, input_hash=NULL, output_hash=NULL) {
  entry <- list("process" = process, "output" = output)
  optional <- list(
    func = func, params = params, time = time, duration = duration,
    input_hash = input_hash, output_hash = output_hash
  )
  c(entry, optional[!vapply(optional, is.null, logical(1))])
}

.addProcess <- function(object, process, output=NULL, duration=NULL, func=NULL,
                        params=NULL, time=NULL, input_hash=NULL, output_hash=NULL) {
  entry <- .processRecord(process, output, duration, func, params, time, input_hash, output_hash)
  object@processing <- c(object@processing, list(entry))
  return(object)
}

#' Add a process to a Tagged Wave or WaveMC object
#'
#' This function takes a `TaggedWave` or `TaggedWaveMC` object and adds a process
#' to the `processing` slot. This is used to keep a record of the processes that
#' have been applied to the object.
#'
#' This is the low-level step used by [tagApply()], which is the function to
#' use to record a process.
#' @param object An object.
#' @param process A description of the process.
#' @param output The output of the process.
#' @param duration The duration of the process in seconds.
#' @param func Optional. The name of the function that performed the process.
#' @param params Optional. A list of the arguments the function was called with.
#' @param time Optional. When the process was run (a `POSIXct` time).
#' @param input_hash Optional. A digest of the object before the process.
#' @param output_hash Optional. A digest of the object after the process.
#' @return The object with the process added.
#' @docType methods
#' @rdname addProcess-methods
#' @keywords internal
setGeneric("addProcess", function(object, process, output=NULL, duration=NULL,
                                  func=NULL, params=NULL, time=NULL,
                                  input_hash=NULL, output_hash=NULL)
  standardGeneric("addProcess") )

#' @rdname addProcess-methods
#' @aliases addProcess,TaggedWave-method
setMethod("addProcess", signature(object = "TaggedWave"), .addProcess)

#' A S4 class for tagged multi-channel waves
#'
#' The TaggedWaveMC class extended the WaveMC class from the tuneR package so
#' that it can include extended metadata and the results of analyses.
#'
#' @slot origin The origin of the object.
#' @slot metadata A list for storing metadata.
#' @slot processing A list recording, in order, each process applied to the object.
#' @slot spectrogram_maad_power A scikit-maad power spectrogram.
setClass(
  "TaggedWaveMC",
  contains="WaveMC",
  slots=.tagSlots()
)

#' @rdname addProcess-methods
#' @aliases addProcess,TaggedWave-method
setMethod("addProcess", signature(object = "TaggedWaveMC"), .addProcess)

#' Tag a Wave or WaveMC object
#'
#' This function takes a `Wave`/`WaveMC` object (or a list of such objects) and
#' returns a corresponding tagged version (`TaggedWave` or `TaggedWaveMC`).
#' @param w A `Wave` or `WaveMC` object (or list of such objects).
#' @param origin The origin of the object (default "user").
#' @return A `TaggedWave` or `TaggedWaveMC` object (or list of such objects).
#' @importFrom methods as
#' @export
tagWave <- function(w, origin="user") {
  if (is(w, "TaggedWave") | is(w, "TaggedWaveMC")) {
    return(w)
  } else if (is(w, "Wave")) {
    validateIsWave(w)
    tw <- as(w, "TaggedWave")
    tw@origin <- origin
    return(tw)
  } else if (is(w, "WaveMC")) {
    validateIsWaveMC(w)
    tw <- as(w, "TaggedWaveMC")
    tw@origin <- origin
    return(tw)
  } else if (is(w, "list")) {
    if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
      return(lapply(w, tagWave))
    } else {
      stop("All items in list must be Wave or WaveMC objects.")
    }
  }
  stop("Attempting to tag object that is not of type Wave or WaveMC.")
}

#' Untag a TaggedWave or TaggedWaveMC object
#'
#' This function takes a TaggedWave/TaggedWaveMC object (or a list of such
#' objects) and returns a corresponding Wave/WaveMC object (or list of such
#' objects).
#' @param w A TaggedWave or TaggedWaveMC object (or list of such objects).
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
  }
  if (is(w, "TaggedWaveMC")) {
    return(as(w, "WaveMC"))
  }
  if (is(w, "Wave") | is(w, "WaveMC")) {
    return(w)
  }
  if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
    return(lapply(w, untagWave))
  }
  stop("Attempting to untag object that is not of type TaggedWave or TaggedWaveMC.")
}

#' Helper function to check if a Wave-like object is tagged
#' @param w A Wave-like object
#' @return A logical value
#' @keywords internal
#' @noRd
.isTagged <- function(w) {
  if (is(w, "TaggedWave") | is(w, "TaggedWaveMC")) {
    return(TRUE)
  }
  if (is(w, "Wave") | is(w, "WaveMC")) {
    return(FALSE)
  }
  if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
    return(sapply(w, .isTagged))
  }
  stop("Attempting to check object that is not Wave like.")
}

#' Helper function to get tags from a TaggedWave or TaggedWaveMC object
#' @param w A TaggedWave or TaggedWaveMC object
#' @return A list of tags
#' @keywords internal
#' @noRd
.getTags <- function(w) {
  if (is(w, "TaggedWave") | is(w, "TaggedWaveMC")) {
    return(list("origin"=w@origin, "metadata"=w@metadata, "processing"=w@processing, spectrogram_maad_power=w@spectrogram_maad_power))
  }
  if (!is(w, "list")) {
    stop("Attempting to get tags from object that is not TaggedWave or TaggedWaveMC.")
  }
  if (all(sapply(w, inherits, what=c("TaggedWave", "TaggedWaveMC")))) {
    tags <- sapply(w, .getTags, simplify=FALSE)
    return(tags)
  }
}
