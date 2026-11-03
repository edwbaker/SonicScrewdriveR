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

#' Name of a function for a process record, from the function alone
#'
#' A function that was stored rather than called, as a filter's is, comes with
#' no expression to read its name from as `.funcName()` does. It is looked for
#' among the bindings of the package it was written in, or of the global
#' environment for one written there, and named as it would be called from
#' outside: `pkg::name` if the package exports it and `pkg:::name` if not. A
#' function bound to no name there, as one written inline is, is anonymous.
#'
#' Functions are compared by what they are rather than by where they are held,
#' so two names for one function, or two functions written alike in one
#' package, cannot be told apart: base R's `identity` and `dontCheck` are both
#' `function(x) x`. Either name says what ran. An exported name is preferred to
#' one that is not, and then the first in alphabetical order.
#' @param fun A function.
#' @return A single string.
#' @keywords internal
#' @noRd
.functionName <- function(fun) {
  top <- if (is.primitive(fun)) .BaseNamespaceEnv else topenv(environment(fun))
  bound <- mget(ls(top, all.names=TRUE, sorted=TRUE), envir=top, inherits=FALSE)
  found <- names(bound)[vapply(bound, identical, logical(1), fun)]
  if (length(found) == 0) {
    return("<anonymous function>")
  }
  if (!isNamespace(top)) {
    return(found[1])
  }
  pkg <- getNamespaceName(top)[[1]]
  exported <- found[found %in% getNamespaceExports(top)]
  if (length(exported) > 0) {
    return(paste0(pkg, "::", exported[1]))
  }
  return(paste0(pkg, ":::", found[1]))
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
  pkg <- .funPackage(fun)
  !is.null(pkg) && !identical(pkg, .ownPackage())
}

#' The package a function was defined in
#'
#' Read from the namespace the function's environment belongs to, so a function
#' is placed by where it was written rather than by the name it was called
#' under. A primitive has no environment to read.
#' @param fun A function.
#' @return The name of the package, or `NULL` for a primitive or for a function
#'   defined outside any package.
#' @keywords internal
#' @noRd
.funPackage <- function(fun) {
  env <- environment(fun)
  if (is.null(env)) {
    return(NULL)
  }
  top <- topenv(env)
  if (!isNamespace(top)) {
    return(NULL)
  }
  return(getNamespaceName(top)[[1]])
}

#' The name of this package
#' @keywords internal
#' @noRd
.ownPackage <- function() {
  getNamespaceName(topenv(environment(.ownPackage)))[[1]]
}

#' Versions of the packages a process record depends on
#'
#' The same call can give a different answer under another version of the
#' package it comes from, so a record that does not say which version ran
#' cannot be repeated with confidence. This package's own version is recorded
#' too, as it decides what the wave a function is given looks like and how the
#' record is made. The versions are those loaded in the session that ran the
#' call, which after an update without a restart are not the ones installed. A
#' primitive is part of R, so is recorded as `base`, whose version is R's.
#' @param fun The function being applied.
#' @return A named character vector of versions, the function's package first.
#' @keywords internal
#' @noRd
.recordVersions <- function(fun) {
  pkg <- if (is.primitive(fun)) "base" else .funPackage(fun)
  pkgs <- unique(c(pkg, .ownPackage()))
  return(vapply(
    pkgs, function(p) getNamespaceVersion(asNamespace(p))[[1]], character(1)
  ))
}

#' Is an object a single tagged wave?
#' @keywords internal
#' @noRd
.isTaggedOne <- function(x) {
  inherits(x, c("TaggedWave", "TaggedWaveMC"))
}

#' Which digest a process record should use
#'
#' The default is the sketch, because a full digest reads every sample - about a
#' second per minute of stereo 48 kHz audio - and is paid twice per operation,
#' on the wave going in and the wave coming out. An hour-long recording would
#' therefore be minutes of hashing per step, which is enough to make a record
#' not worth keeping; a sketch costs the same whatever the length.
#'
#' What that gives up is stated plainly in [tagApply()] and recorded in every
#' record: a sketch of a recording too long to read whole tells an edited one
#' from an unedited one, but is not evidence that two recordings are the same.
#' `options(sonicscrewdriver.provenance="full")` buys that evidence back at the
#' cost above.
#' @return "sketch" or "full".
#' @keywords internal
#' @noRd
.hashMethod <- function() {
  method <- getOption("sonicscrewdriver.provenance", "sketch")
  if (length(method) != 1 || !is.character(method) ||
      !method %in% c("full", "sketch")) {
    warning(
      "sonicscrewdriver.provenance must be 'sketch' or 'full'; using 'sketch'.",
      call.=FALSE
    )
    return("sketch")
  }
  return(method)
}

#' The parts of a wave that a digest covers
#'
#' The samples and the sampling parameters, not the R object, so the digest does
#' not depend on the R version or on any tags.
#' @param w A Wave-like object.
#' @return A list of numeric vectors.
#' @keywords internal
#' @noRd
.waveParts <- function(w) {
  if (is(w, "WaveMC")) {
    return(list(as.numeric(w@.Data), dim(w@.Data), c(w@samp.rate, w@bit)))
  }
  return(list(w@left, w@right, c(w@samp.rate, w@bit)))
}

#' Digest of one numeric vector
#'
#' The bytes are hashed as they are, rather than as a serialised R object, so
#' that the digest of a recording is the same whatever version of R wrote it:
#' `serialize=FALSE` is what keeps digest from wrapping them first.
#' @param x A numeric vector.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.digestNumeric <- function(x) {
  digest::digest(writeBin(as.numeric(x), raw()), algo="sha256", serialize=FALSE)
}

#' Evenly spaced blocks of a long vector
#'
#' Enough of a channel to tell it from an edited one at a cost that does not
#' grow with its length. The blocks begin at the first sample and end at the
#' last, so a recording trimmed at either end is covered. A vector short enough
#' that the blocks would cover it is returned whole.
#' @param x A numeric vector.
#' @param n Samples per block.
#' @param blocks How many blocks.
#' @return A numeric vector.
#' @keywords internal
#' @noRd
.waveSketch <- function(x, n=4096, blocks=10) {
  len <- length(x)
  if (len <= n * blocks) {
    return(x)
  }
  starts <- round(seq(1, len - n + 1, length.out=blocks))
  return(x[unlist(lapply(starts, function(s) seq.int(s, s + n - 1)))])
}

#' Digest of a Wave-like object
#'
#' Each part of the wave is digested on its own and those digests are then
#' digested together, rather than the samples of every channel being
#' concatenated into one vector of bytes first. It is as hard to collide, as
#' every part is covered by a digest of its own and the hex strings joined are
#' each of a fixed length, but nothing the size of the recording is built to get
#' it. Concatenating the bytes cost about 16 bytes per sample on top of the wave
#' itself, which is gigabytes for an hour of stereo audio, and the digest of a
#' five minute stereo recording could fail for want of memory.
#'
#' `method="sketch"`, the default, digests the length of each part and a fixed
#' number of evenly spaced blocks of it rather than all of it, which takes the
#' same time whatever the recording's length. A part short enough that the
#' blocks cover it is still digested whole, so for a recording of up to some
#' seconds a sketch reads every sample and is as strong as a full digest. Past
#' that it tells an edited recording from an unedited one, but is not evidence
#' that two recordings are the same, so a record says which was used.
#' @param w A `Wave`, `WaveMC`, `TaggedWave` or `TaggedWaveMC` object.
#' @param method "full" or "sketch".
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.waveHash <- function(w, method=.hashMethod()) {
  parts <- .waveParts(w)
  if (identical(method, "sketch")) {
    #The lengths go in as well, so that a part the blocks happen to miss a
    #change in is still told apart by how long it is.
    parts <- c(
      list(vapply(parts, length, numeric(1))),
      lapply(parts, .waveSketch)
    )
  }
  return(digest::digest(
    paste0(vapply(parts, .digestNumeric, character(1)), collapse=""),
    algo="sha256", serialize=FALSE
  ))
}

#' Deparse an expression for a record, capped in length
#' @param x An expression.
#' @param max The longest string to keep whole.
#' @return A single string.
#' @keywords internal
#' @noRd
.shortDeparse <- function(x, max=120) {
  text <- paste(deparse(x), collapse=" ")
  if (nchar(text) > max) {
    text <- paste0(substr(text, 1, max - 3), "...")
  }
  return(text)
}

#' Match a call against the formals of the function being applied
#'
#' The wave is the first argument of the call being described, so it takes part
#' in the matching: which formal it filled is then known rather than assumed,
#' and a function given the wave somewhere other than its first argument is
#' described as it really ran. Each argument meant for the function stands in
#' the call as a numbered placeholder, so that the formal it filled can be read
#' back off the match.
#'
#' A call reaching here has already been made, so `match.call()` agreeing with R
#' is the ordinary case. `NULL` for a call it cannot match leaves the caller to
#' fall back on the names as they were given.
#' @param shape A function with the formals to match against.
#' @param supplied The arguments meant for it, as a list.
#' @return The matched call, or `NULL`.
#' @keywords internal
#' @noRd
.matchedCall <- function(shape, supplied) {
  given <- names(supplied)
  if (is.null(given)) {
    given <- rep("", length(supplied))
  }
  slots <- lapply(seq_along(supplied), function(i) as.name(paste0("..", i)))
  names(slots) <- given
  return(tryCatch(
    match.call(
      definition=shape,
      call=as.call(c(list(as.name("fun"), as.name("..wave")), slots)),
      expand.dots=TRUE
    ),
    error=function(e) NULL
  ))
}

#' Name the arguments of a call as the function being applied names them
#'
#' An argument given by position, or by an abbreviation of a formal's name,
#' reaches `tagApply()` under none of the function's names, so a record made
#' from the names as given says only that something was passed: `tagApply(w, f,
#' 3000)` recorded `3000` and not which setting it was. The name of the formal
#' an argument filled is what a reader needs to make the call again.
#' @param matched The call as `.matchedCall()` matched it.
#' @param supplied The arguments meant for it, as a list.
#' @return The same values, named as the formals they filled.
#' @keywords internal
#' @noRd
.matchParams <- function(matched, supplied) {
  parts <- as.list(matched)[-1]
  filled <- match(
    vapply(parts, function(p) paste(deparse(p), collapse=""), character(1)),
    paste0("..", seq_along(supplied))
  )
  names_out <- names(parts)
  if (is.null(names_out)) {
    #A function whose formals are nothing but `...` names none of them.
    names_out <- rep("", length(parts))
  }
  kept <- !is.na(filled)
  values <- supplied[filled[kept]]
  labels <- names_out[kept]
  if (!any(nzchar(labels))) {
    #A function that names none of its arguments leaves a record of them as a
    #bare list, rather than one of empty names.
    names(values) <- NULL
    return(values)
  }
  names(values) <- labels
  return(values)
}

#' The formals a call left to their defaults
#'
#' A default is part of what happened: a filter run at the window length its
#' function happens to prefer was run at that window length, and a record that
#' says nothing of it cannot be repeated. `seewave::ffilter()` has twelve
#' defaults, three of which change the samples it returns.
#'
#' They are kept as the function writes them rather than as values, because a
#' default can be an expression over the other arguments - `pcm = object@pcm` in
#' `tuneR::normalize()` - whose value is known only inside the call, and
#' evaluating it from outside would either fail or quietly give a different
#' answer. For a default that is a literal the two are the same thing. Where a
#' function chooses among a vector of them with `match.arg()`, the first element
#' of the vector recorded is the one that ran.
#' @param shape A function with the formals to read the defaults from.
#' @param matched The call as `.matchedCall()` matched it.
#' @param deparser How each default is turned into text: capped in length for a
#'   record, and whole for a pipeline's id, where two defaults that differ only
#'   past the cap must still be told apart.
#' @return A named list of deparsed defaults, or `NULL` if there are none.
#' @keywords internal
#' @noRd
.argDefaults <- function(shape, matched, deparser=.shortDeparse) {
  formal_args <- as.list(formals(shape))
  if (length(formal_args) == 0) {
    return(NULL)
  }
  #Every formal the call filled, the wave's among them: the wave is ordinarily
  #the first, but it is whichever one it was matched to, and a formal something
  #was passed to was not left to its default.
  formal_args <- formal_args[
    !names(formal_args) %in% c("...", names(as.list(matched))[-1])
  ]
  kept <- vapply(
    formal_args, function(d) !identical(d, quote(expr=)), logical(1)
  )
  if (!any(kept)) {
    return(NULL)
  }
  return(lapply(formal_args[kept], deparser))
}

#' Describe a call for a process record
#'
#' What the caller chose and what the function chose for it, told apart, since a
#' reader needs to know which is which and a record that merged them would say
#' neither. A function that takes its arguments without matching them against
#' formals - as a handful of primitives do - has nothing to match or to default,
#' so its arguments are described as they were given, as are those of a call
#' that cannot be matched.
#' @param fun The function being applied.
#' @param supplied The arguments meant for it, as a list.
#' @param deparser How each default is turned into text, as for
#'   `.argDefaults()`.
#' @return A list of `params` and `defaults`.
#' @keywords internal
#' @noRd
.describeCall <- function(fun, supplied, deparser=.shortDeparse) {
  #args() carries the formals of a primitive, which has none of its own, and is
  #not a function for the few that have no formals to carry.
  shape <- args(fun)
  if (!is.function(shape)) {
    return(list(params=supplied, defaults=NULL))
  }
  matched <- .matchedCall(shape, supplied)
  if (is.null(matched)) {
    return(list(params=supplied, defaults=NULL))
  }
  return(list(
    params=.matchParams(matched, supplied),
    defaults=.argDefaults(shape, matched, deparser)
  ))
}

#' When a process ran, and on what clock
#'
#' A `POSIXct` is an instant, so the time a record held was never wrong, but it
#' is shown in the zone of whoever reads it and the zone is gone altogether once
#' a record is written out as text: the same record read in London and in Tokyo
#' gave times nine hours apart. The zone is named in the record, and set on the
#' time itself so that printing a record gives the clock the machine that ran it
#' was keeping.
#' @return A list of `time` and `timezone`.
#' @keywords internal
#' @noRd
.recordTime <- function() {
  now <- Sys.time()
  zone <- Sys.timezone()
  if (length(zone) != 1 || is.na(zone) || !nzchar(zone)) {
    #Sys.timezone() gives NA where the zone cannot be named, and the
    #abbreviation the clock is keeping is less use than a name but better than
    #nothing. It is not set on the time, as it is not something R can read back.
    return(list(time=now, timezone=format(now, "%Z")))
  }
  attr(now, "tzone") <- zone
  return(list(time=now, timezone=zone))
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
#' record of the call to the `processing` slot of the result. Untagged waves are
#' processed without any record being made, so `tagApply()` can be used safely
#' in code that handles either.
#'
#' A record is a named list holding what is needed to say what was done, with
#' what, when and to which audio:
#'
#' \describe{
#'   \item{`process`}{The label, which is the function's name unless one was
#'     given.}
#'   \item{`func`}{The function as it was called, as a name or `pkg::name`.}
#'   \item{`versions`}{The versions of the package the function comes from and
#'     of this package, as a named character vector, since the same call can
#'     give a different answer under another version of either. They are the
#'     versions loaded in the session that ran the call. A primitive is
#'     recorded as `base`, whose version is R's, and a function defined outside
#'     any package has only this package's.}
#'   \item{`params`}{The arguments the caller supplied, under the names of the
#'     formals they filled rather than the names they were passed under, so an
#'     argument given by position or by an abbreviation is still named.}
#'   \item{`defaults`}{The arguments the caller did not supply and the function
#'     therefore chose, as the function defines them. A default is part of what
#'     happened, and three of `seewave::ffilter()`'s twelve change the samples
#'     it returns. They are deparsed rather than evaluated, because a default
#'     can be an expression over the other arguments whose value is known only
#'     inside the call; for a literal the two are the same. Absent where a
#'     function has no defaults left to it, as the functions of this package do
#'     not, passing their arguments on in full.}
#'   \item{`time`, `timezone`}{When it ran, and the zone the clock was keeping.
#'     The zone is named because a `POSIXct` alone is shown in the zone of
#'     whoever reads it, and loses the zone entirely once written out as text.}
#'   \item{`duration`}{How long the call took, in seconds.}
#'   \item{`input_hash`, `output_hash`, `hash_method`}{Digests of the samples
#'     going in and coming out, and which digest they are. These are what make
#'     the rest checkable: a history can be tested against the audio it claims
#'     to describe rather than taken on trust.}
#' }
#'
#' @section How much the digests prove:
#' A full digest reads every sample, which is about a second per minute of
#' stereo 48 kHz audio and is paid on the wave going in and the wave coming out
#' of every step. An hour-long recording would be minutes of hashing per step,
#' so the default is `"sketch"`: the length of each channel and a fixed number
#' of evenly spaced blocks of it, at the same cost whatever the length.
#'
#' A channel short enough that the blocks cover it is digested whole, so for a
#' recording of up to some seconds a sketch reads every sample. Past that it
#' does not, and the difference matters: a sketch will tell an edited recording
#' from an unedited one, but two recordings that agree on a sketch are not
#' thereby shown to be the same, as a change between the blocks is not read. If
#' a history has to be evidence rather than a good indication, set
#' `options(sonicscrewdriver.provenance="full")` and pay for it.
#'
#' Either way `hash_method` records which was used, so a digest is never taken
#' for more than it is worth. An option is not carried to the workers of a `cl`
#' cluster that does not inherit it, which use the default and record that they
#' did.
#'
#' If `fun` returns a wave without tags, the `origin`, `metadata` and
#' `processing` of `w` are carried over to it. The `spectrogram_maad_power` slot
#' is not carried over, as it no longer matches the signal.
#'
#' If `fun` returns something that is not a wave, such as a number or a data
#' frame, there is no tagged object to hold the record. The result is returned
#' with an attribute `processing` holding the history of `w` followed by the new
#' record, which is complete but for an output digest, as no wave came out, and
#' its `output`, which is left for whatever keeps the result to fill in. A
#' `NULL` result is returned as it is, as it cannot carry an attribute.
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
#' @seealso [flattenOutput()], which turns the output a record holds into rows.
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
#' @param .described The function the record describes, whose formals name the
#'   arguments and give the defaults, and whose package's version is recorded.
#'   It is `.fun` unless `.fun` only wraps the function that does the work, as
#'   it does for a filter. It comes after `...`, so has to be named in full.
#' @param .recorded A named list of arguments to record ahead of those in `...`,
#'   which `.fun` is not given: what a filter is, beside its settings.
#' @details While a process is being recorded further calls are not recorded,
#'   so that only the outermost call makes a record.
#' @keywords internal
#' @noRd
.tagApplyOne <- function(.w, .fun, .func, .label, .untag, ..., .described = .fun,
                         .recorded = NULL) {
  # Every formal is named with a dot for the same reason tagApply()'s are: the
  # arguments meant for .fun arrive in `...`, and R would match a short name
  # against the formals before it.
  #
  # tagApply() refuses anything that is not a wave, or a list of them, before
  # this is reached, so a caller of its own has to do the same.
  ran <- .tagApplyRecord(
    .w, .fun, .func, .label, .untag, ..., .described = .described,
    .recorded = .recorded
  )
  return(.keepRecord(.w, ran$result, ran$record))
}

#' Apply a function to one wave, giving its result and its record apart
#'
#' What `.tagApplyOne()` does, short of putting the record anywhere. A result
#' that is not a wave carries its record as an attribute, which `NULL` cannot
#' carry, so a caller that needs the record of every call - a pipeline, which
#' records that a step ran even where it found nothing - is given the two
#' together instead. Handing them back is what lets that work on the workers of
#' a cluster, where state held anywhere else would not come back.
#'
#' A function of this package that records itself, as [filterWave()] does,
#' makes a better record than one made around it can: filterWave() names its
#' filter's function and settings, where a record around it holds only the
#' filter object. Only the outermost call ordinarily records, so `.selfRecord`
#' lets the function's own record be made, and makes none here where it was.
#'
#' @param .w,.fun,.func,.label,.untag,...,.described,.recorded As for
#'   `.tagApplyOne()`.
#' @param .selfRecord Whether `.fun` may make its own record. Where it gives
#'   back a tagged wave whose history is longer than that of `.w`, it did, and
#'   no record is made here.
#' @return A list of `result`, what `.fun` gave back, and `record`, its record,
#'   complete but for `output` and `output_hash`. `record` is `NULL` where none
#'   was made: for an untagged wave, inside a call already being recorded, and
#'   for a function that recorded itself.
#' @keywords internal
#' @noRd
.tagApplyRecord <- function(.w, .fun, .func, .label, .untag, ..., .described = .fun,
                            .recorded = NULL, .selfRecord = FALSE) {
  if (is.na(.untag)) {
    .untag <- .needsUntag(.fun)
  }
  if (!.isTagged(.w) || isTRUE(.recState$active)) {
    return(list(result=.fun(if (.untag) untagWave(.w) else .w, ...), record=NULL))
  }
  if (!.selfRecord) {
    .recState$active <- TRUE
    on.exit(.recState$active <- FALSE)
  }
  method <- .hashMethod()
  started <- .recordTime()
  input_hash <- .waveHash(.w, method)
  res <- .fun(if (.untag) untagWave(.w) else .w, ...)
  duration <- as.numeric(difftime(Sys.time(), started$time, units = "secs"))
  if (.selfRecord && .isTaggedOne(res) &&
      length(res@processing) > length(.w@processing)) {
    return(list(result=res, record=NULL))
  }
  #Described after the function has run, so that a call that fails costs nothing
  #to describe. The arguments are already evaluated by then, so naming them does
  #not run anything a second time.
  call <- .describeCall(.described, list(...))
  record <- .processRecord(
    if (is.null(.label)) .func else .label,
    duration = duration,
    func = .func,
    versions = .recordVersions(.described),
    params = .summariseParams(c(.recorded, call$params)),
    defaults = call$defaults,
    time = started$time,
    timezone = started$timezone,
    input_hash = input_hash,
    hash_method = method
  )
  return(list(result=res, record=record))
}

#' Keep a record with the result it describes
#'
#' A wave takes the record into its history, and is given the tags of the wave
#' it was made from where it came back without them. Anything else carries the
#' history of the wave it was made from, and the record, as an attribute, which
#' `NULL` cannot carry.
#' @param .w The wave the function was applied to.
#' @param res What the function gave back.
#' @param record The record `.tagApplyRecord()` made, or `NULL` for none.
#' @return `res`, holding the record.
#' @keywords internal
#' @noRd
.keepRecord <- function(.w, res, record) {
  if (is.null(record)) {
    return(res)
  }
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
  record$output_hash <- .waveHash(res, record$hash_method)

  res@processing <- c(res@processing, list(record))
  res
}
