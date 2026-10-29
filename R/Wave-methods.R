.concat <- function(object, ..., method) {
  .validateChoice(method, c("bind", "noClick"), msg="Unknown concatenation method.")
  #The waves that were concatenated on reach the record as the arguments of the
  #call, which keeps the tags of each of them as well as a digest. The process
  #keeps the name it has always had, so a history written before this reads the
  #same as one written after it.
  return(.tagApplyOne(
    object, .concatWaves, "concat", paste("concat", method, sep="-"), TRUE,
    ..., method=method
  ))
}

#' Concatenate waves without recording it
#'
#' The first wave is named with a dot for the same reason `.tagApplyOne()`'s
#' arguments are: the waves to concatenate on arrive in `...`, and R would match
#' a short name against the formals before it.
#' @keywords internal
#' @noRd
.concatWaves <- function(.object, ..., method) {
  if (all(sapply(list(.object, ...), inherits, "Wave"))) {
    allobjects <- untagWave(c(list(.object), ...))
    if (method=="noClick") {
      allobjects <- .noClick(allobjects)
    }
    return(do.call(tuneR::bind, allobjects))
  }
  if (all(sapply(list(.object, ...), inherits, "WaveMC"))) {
    allobjects <- untagWave(c(list(.object), list(...)))
    if (method=="noClick") {
      allobjects <- .noClick(allobjects)
    }
    return(do.call(tuneR::bind, allobjects))
  }
  #Mixed classes match neither branch above. A tagged first wave was returned
  #unchanged, with a record of a concatenation that had not happened.
  stop("All objects must be Wave-like objects of the same type.")
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
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()]. The waves that
#' were concatenated on are held in the arguments of the record, each with its
#' own tags and a digest of its samples.
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
