.concat <- function(object, ..., method) {
  if (!method %in% c("bind")) {
    stop("Unknown concatenation method.")
  }
  objectTagged <- .isTagged(object)
  if (all(sapply(list(object, ...), inherits, c("Wave")))) {
    object2 <- tuneR::bind(untagWave(object), untagWave(...))
    if (objectTagged) {
      object@left <- object2@left
      object@right <- object2@right
      rm(object2)
    } else {
      return(object2)
    }
  }
  if (all(sapply(list(object, ...), inherits, c("WaveMC")))) {
    object2 <- tuneR::bind(untagWave(object), untagWave(...))
    if (objectTagged) {
      object@`.Data` <- object2@`.Data`
      rm(object2)
    } else {
      return(object2)
    }
  }

  # If tagged then get slots of objects concatenated
  if (objectTagged) {
    object <- addProcess(object, "concat", .getTags(list(...)))
  }

  return(object)
}

#' Concatenate two or more Wave-like objects.
#'
#' The `concat()` method is a more flexible version of the `bind()` method
#' from `tuneR` package, that allows specifying more advanced types of
#' concatenation.
#'
#' @param object A Wave like object.
#' @param ... Wave like objects to concatenate to object.
#' @param method One of "bind".
#' @return A concatenated Wave like object, with type of `object`.
#' @docType methods
#' @rdname concat-methods
#' @export
setGeneric("concat", function(object, ..., method="bind")
  standardGeneric("concat") )

#' @rdname concat-methods
#' @aliases concat,Wave-method
setMethod("concat", signature(object = "Wave"), .concat)

#' @rdname concat-methods
#' @aliases concat,WaveMC-method
setMethod("concat", signature(object = "WaveMC"), .concat)

#' @rdname concat-methods
#' @aliases concat,TaggedWave-method
setMethod("concat", signature(object = "TaggedWave"), .concat)

#' @rdname concat-methods
#' @aliases concat,TaggedWaveMC-method
setMethod("concat", signature(object = "TaggedWaveMC"), .concat)
