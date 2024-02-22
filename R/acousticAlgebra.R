.concat <- function(object, ..., method) {
  if (!method %in% c("bind", "noClick")) {
    stop("Unknown concatenation method.")
  }
  objectTagged <- .isTagged(object)
  if (all(sapply(list(object, ...), inherits, "Wave"))) {
    allobjects <- untagWave(c(list(object), ...))
    if (method=="noClick") {
      allobjects <- .noClick(allobjects)
    }
    object2 <- do.call(tuneR::bind, allobjects)
    if (objectTagged) {
      object@left <- object2@left
      object@right <- object2@right
      rm(object2)
    } else {
      return(object2)
    }
  }
  if (all(sapply(list(object, ...), inherits, "WaveMC"))) {
    allobjects <- untagWave(c(list(object), list(...)))
    if (method=="noClick") {
      allobjects <- .noClick(allobjects)
    }
    object2 <- do.call(tuneR::bind, allobjects)
    if (objectTagged) {
      object@`.Data` <- object2@`.Data`
      rm(object2)
    } else {
      return(object2)
    }
  }

  # If tagged then get slots of objects concatenated
  if (objectTagged) {
    object <- addProcess(object, paste("concat", method, sep="-"), .getTags(list(...)))
  }

  return(object)
}

.noClick <- function(objects) {
  if (!is(objects, "list")) {
    stop("Input must be a list.")
  }
  if (length(objects) == 1) {
    return(objects)
  }
  objects[[1]] <- tuneR::prepComb(objects[[1]], where="end")
  objects[[length(objects)]] <- tuneR::prepComb(objects[[length(objects)]], where="start")
  if (length(objects) == 2) {
    return(objects)
  }
  objects[2:(length(objects)-1)]<- lapply(objects[2:(length(objects)-1)], tuneR::prepComb, where="both")
  return(objects)
}

#' Concatenate two or more Wave-like objects.
#'
#' The `concat()` method is a more flexible version of the `bind()` method
#' from `tuneR` package, that allows specifying more advanced types of
#' concatenation. Setting `method` to "noClick" will remove any click between
#' Wave objects caused by sudden jumps in amplitude by applying `tuneR::prepComb()`
#' appropriately with default value of zero (this is only effective for the left
#' channel or stereo or multi-channel recordings).
#'
#' @param object A Wave like object.
#' @param ... Wave like objects to concatenate to object.
#' @param method One of "bind", "noClick". Default is "bind".
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

.lengthWave <- function(object) {
  return(length(object@left))
}

.lengthWaveMC <- function(object) {
  return(nrow(object@`.Data`))
}
