# Set while a process is being recorded, so that only the outermost call records.
.recState <- new.env()

#' Name of a function for a process record
#'
#' Uses the name the function was given, or the name it was called by if that
#' is a plain name or `pkg::name`, and otherwise a placeholder.
#' @param fun The function, or its name.
#' @param expr The unevaluated argument that `fun` came from.
#' @keywords internal
#' @noRd
.funcName <- function(fun, expr) {
  if (is.character(fun)) {
    return(fun)
  }
  if (is.name(expr) || (is.call(expr) && as.character(expr[[1]])[1] %in% c("::", ":::"))) {
    return(paste(deparse(expr), collapse = ""))
  }
  "<anonymous function>"
}

#' Does a function need to be given an untagged wave?
#'
#' Functions from other packages, such as seewave, check the class of a wave
#' with `class(w)[1] == "Wave"` and so reject the tagged classes. Functions from
#' this package, and functions defined by the user, are assumed to cope.
#' @param fun A function.
#' @return A logical value.
#' @keywords internal
#' @noRd
.needsUntag <- function(fun) {
  env <- environment(fun)
  if (is.null(env)) {
    return(FALSE)
  }
  top <- topenv(env)
  isNamespace(top) && !identical(top, topenv(environment(.needsUntag)))
}

#' Is an object a single tagged wave?
#' @keywords internal
#' @noRd
.isTaggedOne <- function(x) {
  inherits(x, c("TaggedWave", "TaggedWaveMC"))
}

#' Digest of a Wave-like object
#'
#' Hashes the samples and the sampling parameters, not the R object, so the
#' result does not depend on the R version or on any tags.
#'
#' The bytes are hashed as they are, rather than as a serialised R object, so
#' that the digest of a recording is the same whatever version of R wrote it:
#' `serialize=FALSE` is what keeps digest from wrapping them first.
#' @param w A `Wave`, `WaveMC`, `TaggedWave` or `TaggedWaveMC` object.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.waveHash <- function(w) {
  if (is(w, "WaveMC")) {
    samples <- list(as.numeric(w@.Data), dim(w@.Data))
  } else {
    samples <- list(w@left, w@right)
  }
  bytes <- unlist(lapply(
    c(samples, list(c(w@samp.rate, w@bit))),
    function(x) writeBin(as.numeric(x), raw())
  ))
  digest::digest(bytes, algo="sha256", serialize=FALSE)
}

#' Summarise arguments for a process record
#'
#' Waves are replaced by their hash and very large objects by a placeholder, so
#' that the record stays small. A tagged wave keeps its hash, origin, metadata
#' and processing, so that the history of a wave that is combined with another
#' is not lost.
#' @param args A list of arguments.
#' @return A list of the same names with bulky items summarised.
#' @keywords internal
#' @noRd
.summariseParams <- function(args) {
  lapply(args, function(x) {
    if (.isTaggedOne(x)) {
      return(list(sha256 = .waveHash(x), origin = x@origin, metadata = x@metadata, processing = x@processing))
    }
    if (inherits(x, c("Wave", "WaveMC"))) {
      return(paste0("<wave sha256:", .waveHash(x), ">"))
    }
    if (is.function(x)) {
      return("<function>")
    }
    if (as.numeric(utils::object.size(x)) > 1e5) {
      return("<large object>")
    }
    x
  })
}

#' Apply a function to a tagged wave and record the process
#'
#' Runs `fun(w, ...)` and, if `w` is a `TaggedWave` or `TaggedWaveMC`, appends a
#' record of the call to the `processing` slot of the result. The record holds
#' the function name, its arguments, the time, the duration and digests of the
#' input and output. Untagged waves are processed without any record being made,
#' so `tagApply()` can be used safely in code that handles either.
#'
#' If `fun` returns a wave without tags, the `origin`, `metadata` and
#' `processing` of `w` are carried over to it. The `spectrogram_maad_power` slot
#' is not carried over, as it no longer matches the signal.
#'
#' If `fun` returns something that is not a wave, such as a number or a data
#' frame, there is no tagged object to hold the record. The result is returned
#' with an attribute `processing` holding the history of `w` followed by the new
#' record (without an output digest). A `NULL` result is returned as it is.
#'
#' Functions from other packages are given an untagged wave. seewave, for
#' example, checks the class of a wave with `class(w)[1] == "Wave"` and so
#' rejects a `TaggedWave`. The tags are put back on a wave that is returned, as
#' described above. Functions from this package and functions defined by the
#' user are given the tagged wave unless `untag = TRUE`.
#'
#' Only the outermost call makes a record. A function that calls `tagApply()`
#' itself, as a function of this package may come to, is recorded once when it
#' is passed to `tagApply()` in turn, under the name and arguments of the outer
#' call rather than twice under both.
#'
#' The wave and the function are named `.w` and `.fun` so that the arguments
#' meant for `.fun` reach it. R matches a named argument against the arguments
#' named before `...` by its first letters as well as in full, so a plain `w`
#' and `fun` would swallow anything called `f`, `fu` or `w` - and `f` is what
#' seewave calls a sampling frequency, which is exactly the sort of call this
#' function is for. `label`, `untag` and `cl` come after `...` and so must be
#' named in full, which leaves only a function with an argument of one of those
#' three names beyond reach.
#'
#' @param .w A `Wave`, `WaveMC`, `TaggedWave` or `TaggedWaveMC` object, or a
#'   list of such objects.
#' @param .fun A function, or the name of one, taking the wave as its first
#'   argument.
#' @param ... Further arguments passed to `.fun`.
#' @param label Optional. A description of the process. Defaults to the name of
#'   the function.
#' @param untag Whether to pass `.fun` an untagged wave (see Details). The
#'   default, `NA`, does so for functions from other packages.
#' @param cl Optional. A cluster, as made by [parallel::makeCluster()], used to
#'   process a list of waves in parallel. The workers need `sonicscrewdriver`
#'   to be available.
#' @return The result of `.fun`, with the process recorded if `.w` is tagged.
#' @export
#' @examples
#' tw <- tagWave(tuneR::sine(440, duration = 44100, samp.rate = 44100))
#' tw <- tagApply(tw, tuneR::normalize, unit = "16")
#' tw@processing[[1]]$func
#'
#' # seewave functions can be used with tagged waves
#' \dontrun{
#' tw <- tagApply(tw, seewave::ffilter, from = 1000, to = 2000, output = "Wave")
#' }
tagApply <- function(.w, .fun, ..., label = NULL, untag = NA, cl = NULL) {
  func_name <- .funcName(.fun, substitute(.fun))
  .fun <- match.fun(.fun)

  if (inherits(.w, c("Wave", "WaveMC"))) {
    return(.tagApplyOne(.w, .fun, func_name, label, untag, ...))
  }
  if (is.list(.w) && all(vapply(.w, inherits, logical(1), what = c("Wave", "WaveMC")))) {
    if (is.null(cl)) {
      return(lapply(.w, .tagApplyOne, .fun, func_name, label, untag, ...))
    }
    return(parallel::parLapply(cl, .w, .tagApplyOne, .fun, func_name, label, untag, ...))
  }
  stop("Can only record a process on a Wave or WaveMC object, or a list of them.")
}

#' Apply a function to one wave and record the process if it is tagged
#' @param .label The label given by the user, or `NULL` to use `.func`.
#' @param .untag `TRUE` or `FALSE`, or `NA` to decide with `.needsUntag()`.
#' @details While a process is being recorded further calls are not recorded,
#'   so that only the outermost call makes a record.
#' @keywords internal
#' @noRd
.tagApplyOne <- function(.w, .fun, .func, .label, .untag, ...) {
  # Every formal is named with a dot for the same reason tagApply()'s are: the
  # arguments meant for .fun arrive in `...`, and R would match a short name
  # against the formals before it.
  #
  # tagApply() refuses anything that is not a wave, or a list of them, before
  # this is reached, so a caller of its own has to do the same.
  if (is.na(.untag)) {
    .untag <- .needsUntag(.fun)
  }
  if (!.isTagged(.w) || isTRUE(.recState$active)) {
    return(.fun(if (.untag) untagWave(.w) else .w, ...))
  }
  .recState$active <- TRUE
  on.exit(.recState$active <- FALSE)
  t0 <- Sys.time()
  input_hash <- .waveHash(.w)
  res <- .fun(if (.untag) untagWave(.w) else .w, ...)
  duration <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  record <- .processRecord(
    if (is.null(.label)) .func else .label,
    duration = duration,
    func = .func,
    params = .summariseParams(list(...)),
    time = t0,
    input_hash = input_hash
  )

  if (!inherits(res, c("Wave", "WaveMC"))) {
    if (is.null(res)) {
      return(res)
    }
    attr(res, "processing") <- c(.w@processing, list(record))
    return(res)
  }

  if (!.isTagged(res)) {
    res <- tagWave(res, origin = .w@origin)
    res@metadata <- .w@metadata
    res@processing <- .w@processing
  }
  record$output_hash <- .waveHash(res)

  res@processing <- c(res@processing, list(record))
  res
}
