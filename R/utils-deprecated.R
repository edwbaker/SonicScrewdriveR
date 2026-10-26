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

#' Replace a forwarded dots argument with the expressions it stands for
#'
#' A wrapper that forwards its own arguments, as `f(...)` or as `lapply(x, f,
#' unit="cm")` does, leaves the dots unexpanded in the call, so an argument
#' passed that way cannot be counted or named from the call itself.
#'
#' @param call The call to rewrite.
#' @param items A named list to splice in where the dots are.
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

#' Which formal each argument of a call is matched to
#'
#' R's own rule, applied to the names a call gives: every exact name first, then
#' the abbreviations that match one remaining formal and no other, then whatever
#' is left over in order. It is written out here rather than taken from
#' `match.call()` because which argument went where is needed by position, and
#' `match.call()` reports the result by formal with no way back.
#'
#' @param given The name each argument was given, empty where it had none.
#' @param formalNames The names of the formals, in order.
#' @return The formal each argument matches, NA where it matches none.
#' @keywords internal
#' @noRd
.matchFormals <- function(given, formalNames) {
  matched <- rep(NA_character_, length(given))
  left <- formalNames

  for (i in which(nzchar(given))) {
    at <- match(given[i], left)
    if (!is.na(at)) {
      matched[i] <- left[at]
      left <- left[-at]
    }
  }
  for (i in which(nzchar(given) & is.na(matched))) {
    hits <- which(startsWith(left, given[i]))
    if (length(hits) == 1) {
      matched[i] <- left[hits]
      left <- left[-hits]
    }
  }
  for (i in which(!nzchar(given))) {
    if (length(left) == 0) {
      break
    }
    matched[i] <- left[1]
    left <- left[-1]
  }
  return(matched)
}

#' The name each argument of a call was given
#'
#' @param call The call, with any dots already spliced in.
#' @return One name per argument, empty where the argument had none.
#' @keywords internal
#' @noRd
.argNames <- function(call) {
  n <- length(call) - 1
  if (n == 0) {
    return(character(0))
  }
  given <- names(call)
  if (is.null(given)) {
    return(rep("", n))
  }
  return(given[-1])
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
#' @return The names of the formals as they were, in order.
#' @keywords internal
#' @noRd
.formalsBefore <- function(target, renamed) {
  nms <- names(formals(target))
  nms <- nms[!nms %in% names(renamed)]
  back <- names(renamed)
  names(back) <- unname(renamed)
  hit <- nms %in% names(back)
  nms[hit] <- back[nms[hit]]
  return(nms)
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
#' Which arguments were supplied is therefore settled from the call, so that a
#' former name is seen however it arrives: in full, abbreviated to a name that
#' matches it and nothing else, or forwarded through another function's dots.
#' Settling that forces nothing, so a call that uses none of the former names
#' behaves exactly as it would without any of this, including the order it
#' evaluates its arguments in.
#'
#' Where a former name was given, each argument is matched a second time against
#' the signature as it was, which puts it where it belonged then, and the
#' function is called again with those arguments. The values handed over are the
#' ones R has already bound in this frame, not the expressions that produced
#' them: an expression is therefore evaluated once however it is reached, which
#' also keeps the environment each argument was written in, and an argument some
#' caller forced in advance, as `forceAndCall()` does, is not evaluated a second
#' time.
#'
#' The former names remain formals, so a misspelled argument is still an error
#' rather than being absorbed by a dots argument. Two things differ while both
#' names are arguments: an abbreviation that matches a former name and the
#' argument that replaced it, such as inpu for both input and input.unit, is
#' ambiguous and raises an error; and a call that gives a former name has its
#' arguments evaluated when it is matched again, rather than when the function
#' first reads each one, so one whose arguments must be evaluated in a
#' particular order should give the current names.
#'
#' @param call The call to the function, from `sys.call()`.
#' @param env The frame the call was made from, from `parent.frame()`.
#' @param renamed A named character vector mapping each former argument name to
#'   the argument that replaced it.
#' @param fn The name of the function, used in the warning.
#' @return NULL if the call used none of the former names, so that the function
#'   carries on with its own body. Otherwise the result of calling the function
#'   again, wrapped in a list so that a NULL result is still told apart from it.
#' @keywords internal
#' @noRd
.withRenamedArgs <- function(call, env, renamed, fn) {
  target <- sys.function(-1)
  frame <- parent.frame()

  #Only the expressions are spliced in to settle which names were given, so
  #that nothing is evaluated on a call that turns out not to need matching
  #again.
  probe <- if (.forwardsDots(call)) .spliceDots(call, .dotExpressions(env)) else call
  given <- .argNames(probe)
  now <- .matchFormals(given, names(formals(target)))

  named <- now[nzchar(given) & !is.na(now)]
  used <- intersect(named, names(renamed))

  #A former name filled by a positional argument, which takes more arguments
  #than the signature had before the rename, was an unused argument then.
  stray <- setdiff(intersect(now[!is.na(now)], names(renamed)), named)
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

  #The signature as it was knows only the former names, so a name that is
  #current is put back to the one it replaced before matching again.
  back <- names(renamed)
  names(back) <- unname(renamed)
  before <- given
  for (i in seq_along(before)) {
    if (!nzchar(before[i]) || is.na(now[i])) {
      next
    }
    before[i] <- if (now[i] %in% names(back)) back[[now[i]]] else now[i]
  }
  then <- .matchFormals(before, .formalsBefore(target, renamed))

  args <- list()
  for (i in seq_along(then)) {
    if (is.na(then[i]) || is.na(now[i])) {
      next
    }
    to <- if (then[i] %in% names(renamed)) renamed[[then[i]]] else then[i]
    #The value as this frame already holds it. An argument already forced is
    #not forced again, and one not yet forced is evaluated here, once, in the
    #environment it was written in.
    args[[to]] <- get(now[i], envir=frame)
  }
  return(list(do.call(target, args, quote=TRUE)))
}
