#' Warn that an argument has been renamed
#'
#' The unit arguments were named input, unit or units depending on which function
#' they belonged to. They are now input.unit for the unit a value is supplied in
#' and output.unit for the unit a result is returned in. The former names are
#' still accepted, by `.withRenamedArgs()`.
#'
#' @param old The deprecated argument name.
#' @param new The argument that replaces it.
#' @param fn The function the argument belongs to.
#' @return No return value, called for its side effect of raising a warning.
#' @keywords internal
#' @noRd
.deprecatedArg <- function(old, new, fn) {
  warning(
    "The ", old, " argument of ", fn, "() is deprecated and will be removed in ",
    "a future release, use ", new, " instead.",
    call.=FALSE
  )
  return(invisible(NULL))
}

#' The expressions a forwarded dots argument stands for
#'
#' Taken with substitute(), so that the arguments are not evaluated. Which
#' arguments a call supplies has to be settled without forcing any of them: a
#' call that uses none of the former names must behave exactly as it would
#' without any of this, down to the order its arguments are evaluated in.
#'
#' @param env The frame that holds the dots.
#' @return A named list of expressions, or NULL if the frame has no dots.
#' @keywords internal
#' @noRd
.dotExpressions <- function(env) {
  if (!.hasDots(env)) {
    return(NULL)
  }
  return(eval(quote(as.list(substitute(list(...)))[-1]), env))
}

#' The values a forwarded dots argument stands for
#'
#' Forced in the frame that holds the dots, where their promises belong. Forcing
#' them there rather than substituting their expressions matters: an expression
#' written in the caller of a wrapper means nothing in the frame the wrapper
#' calls from. This is only used once a former name is known to have been given,
#' since forcing them is a change in when they are evaluated.
#'
#' @param env The frame that holds the dots.
#' @return A named list of values, or NULL if the frame has no dots.
#' @keywords internal
#' @noRd
.dotValues <- function(env) {
  if (!.hasDots(env)) {
    return(NULL)
  }
  #Not guarded: an argument that fails has to fail here, once. Treating it as an
  #absent dots argument would leave it in the call to be evaluated a second
  #time, so one that fails only the first time would appear to succeed.
  return(eval(quote(list(...)), env))
}

#' Replace a forwarded dots argument with what it stands for
#'
#' A wrapper that forwards its own arguments, as `f(...)` or as `lapply(x, f,
#' unit="cm")` does, leaves the dots unexpanded in the call, so a former name
#' passed that way cannot be seen in the call itself.
#'
#' @param call The call to rewrite.
#' @param items A named list to splice in where the dots are, from
#'   `.dotExpressions()` or `.dotValues()`.
#' @return The call with any dots replaced by `items`.
#' @keywords internal
#' @noRd
.spliceDots <- function(call, items) {
  if (is.null(items)) {
    return(call)
  }
  parts <- as.list(call)
  out <- list()
  for (i in seq_along(parts)) {
    if (identical(parts[[i]], quote(...))) {
      out <- c(out, items)
    } else {
      out <- c(out, parts[i])
    }
  }
  return(as.call(out))
}

#' Whether a call forwards a dots argument
#'
#' @param call The call to look at.
#' @return TRUE if the call passes `...` on.
#' @keywords internal
#' @noRd
.forwardsDots <- function(call) {
  return(any(vapply(as.list(call), identical, logical(1), quote(...))))
}

#' Which formals a call supplies by name
#'
#' `match.call()` names every argument, including the ones given positionally,
#' so it cannot say which were named. That distinction is needed: a former name
#' and the argument that replaced it are only in conflict when both were asked
#' for by name, whereas the replacement being filled by a positional argument is
#' the ordinary case the rewriting exists to correct.
#'
#' @param call The call, with any dots already spliced in.
#' @param target The function being called.
#' @return The names of the formals the call supplies by name, abbreviated names
#'   resolved to the formal they match.
#' @keywords internal
#' @noRd
.namedSupplies <- function(call, target) {
  nms <- names(call)
  if (is.null(nms)) {
    return(character(0))
  }
  given <- nms[nzchar(nms)]
  if (length(given) == 0) {
    return(character(0))
  }
  fmls <- names(formals(target))
  #pmatch() takes an exact match before a partial one, as argument matching does.
  resolved <- fmls[pmatch(given, fmls)]
  return(resolved[!is.na(resolved)])
}

#' The signature a function had before its unit arguments were renamed
#'
#' Every new argument took the position of the one it replaced, and the former
#' names were kept as formals at the end of the list, so dropping those and
#' putting the former names back gives the signature as it was.
#'
#' @param target The function as it is now.
#' @param renamed A named character vector mapping each former argument name to
#'   the argument that replaced it.
#' @return A function with the former signature and an empty body, for
#'   `match.call()` to match against.
#' @keywords internal
#' @noRd
.signatureBefore <- function(target, renamed) {
  fmls <- formals(target)
  fmls <- fmls[!names(fmls) %in% names(renamed)]
  back <- names(renamed)
  names(back) <- unname(renamed)
  nms <- names(fmls)
  hit <- nms %in% names(back)
  nms[hit] <- back[nms[hit]]
  names(fmls) <- nms
  before <- function() NULL
  formals(before) <- fmls
  return(before)
}

#' Put the current argument names back to their former spelling
#'
#' The signature as it was before the rename knows only the former names, so a
#' call that mixes a former name with a current one, as `convert2seconds(T=1,
#' input.unit="minutes")` does, cannot be matched against it as written.
#' Resolving each name to a formal also settles any abbreviation.
#'
#' @param call The call to rewrite.
#' @param target The function as it is now.
#' @param renamed A named character vector mapping each former argument name to
#'   the argument that replaced it.
#' @return The call with each supplied name spelled as it was before the rename.
#' @keywords internal
#' @noRd
.namesBefore <- function(call, target, renamed) {
  nms <- names(call)
  if (is.null(nms)) {
    return(call)
  }
  fmls <- names(formals(target))
  back <- names(renamed)
  names(back) <- unname(renamed)
  for (i in seq_along(nms)) {
    if (!nzchar(nms[i])) {
      next
    }
    at <- pmatch(nms[i], fmls)
    if (is.na(at)) {
      next
    }
    nms[i] <- if (fmls[at] %in% names(back)) back[[fmls[at]]] else fmls[at]
  }
  names(call) <- nms
  return(call)
}

#' Accept the former name of a renamed argument
#'
#' A former name cannot occupy the same position as the argument that replaced
#' it, so it is kept as a formal at the end of the list. That frees the position
#' it used to hold, and reading it in the body is not enough: in a call that
#' gives the former name by keyword and later arguments positionally, such as
#' `convert2seconds(T=1, "hours")`, R binds each positional argument one place
#' earlier than it did before the rename, and "hours" arrives as the time rather
#' than its unit.
#'
#' Which arguments were supplied is therefore taken from the call rather than
#' from the body, so that a former name is seen however it arrives: in full,
#' abbreviated to a name that matches it and nothing else, or forwarded through
#' another function's dots. Settling that forces nothing, so a call that uses
#' none of the former names behaves exactly as it would without any of this,
#' including the order it evaluates its arguments in. The arguments are only
#' then matched against the signature as it was before the rename and the former
#' names replaced, so that the rewritten call binds exactly as the original did
#' and the matching is left to R rather than reconstructed.
#'
#' The former names remain formals, so a misspelled argument is still an error
#' rather than being absorbed by a dots argument. Two things do differ while both
#' names are arguments: an abbreviation that matches a former name and the
#' argument that replaced it, such as inpu for both input and input.unit, is
#' ambiguous and raises an error; and a call that reaches a former name through
#' another function's dots has those arguments forced when the call is rewritten,
#' so one whose arguments must be evaluated in a particular order should give the
#' current names.
#'
#' @param call The call to the function, from `sys.call()`.
#' @param env The environment to evaluate the rewritten call in, from
#'   `parent.frame()`.
#' @param renamed A named character vector mapping each former argument name to
#'   the argument that replaced it.
#' @param fn The name of the function, used in the warning.
#' @return NULL if the call used none of the former names, so that the function
#'   carries on with its own body. Otherwise the result of the rewritten call,
#'   wrapped in a list so that a NULL result is still told apart from it.
#' @keywords internal
#' @noRd
.withRenamedArgs <- function(call, env, renamed, fn) {
  target <- sys.function(-1)
  forwards <- .forwardsDots(call)

  #Only the expressions are spliced in to settle which names were given, so
  #that nothing is evaluated on a call that turns out not to need rewriting.
  probe <- if (forwards) .spliceDots(call, .dotExpressions(env)) else call
  named <- .namedSupplies(probe, target)
  used <- intersect(named, names(renamed))

  #A former name filled by a positional argument, which takes more arguments
  #than the signature had before the rename, was an unused argument then.
  matched <- match.call(target, probe, expand.dots=TRUE, envir=env)
  stray <- setdiff(intersect(names(matched), names(renamed)), named)
  if (length(stray) > 0) {
    stop(
      "unused argument", if (length(stray) > 1) "s" else "", " in ", fn, "()",
      call.=FALSE
    )
  }
  if (length(used) == 0) {
    return(NULL)
  }

  clash <- intersect(named, unname(renamed[used]))
  if (length(clash) > 0) {
    stop(
      fn, "() was given both ", paste(used, collapse=", "), " and ",
      paste(clash, collapse=", "), ", which are the same argument. Give only ",
      paste(unique(unname(renamed[used])), collapse=", "), ".",
      call.=FALSE
    )
  }

  for (old in used) {
    .deprecatedArg(old, renamed[[old]], fn)
  }

  #The call is being rewritten from here, so forwarded arguments are taken as
  #values, forced in the frame whose dots hold them.
  rewritten <- if (forwards) .spliceDots(call, .dotValues(env)) else call
  rewritten <- .namesBefore(rewritten, target, renamed)
  before <- match.call(
    .signatureBefore(target, renamed), rewritten, expand.dots=TRUE, envir=env
  )
  nms <- names(before)
  hit <- nms %in% names(renamed)
  nms[hit] <- renamed[nms[hit]]
  names(before) <- nms
  #The function itself, not the expression that chose it: evaluating
  #pick()(1, input="hours") again would call pick() a second time, which may
  #count, fail, or hand back a different function.
  before[[1]] <- target
  return(list(eval(before, env)))
}

#' Whether a frame has a dots argument
#'
#' Asked of the frame itself rather than by trying to read the dots, so that a
#' frame without them is told apart from one whose arguments fail when evaluated.
#'
#' @param env The frame to look at.
#' @return TRUE if the frame has a dots argument.
#' @keywords internal
#' @noRd
.hasDots <- function(env) {
  return(exists("...", envir=env, inherits=FALSE))
}
