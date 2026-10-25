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
#' Rewriting the call with the former names replaced by the new ones, and
#' evaluating that instead, leaves the matching to R. Because each new argument
#' took the position of the one it replaced, the rewritten call binds exactly as
#' the original did. The former names are still formals, so a misspelled
#' argument remains an error rather than being absorbed by a `...`.
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
  given <- names(call)
  if (is.null(given)) {
    return(NULL)
  }
  #Only an exact name is rewritten. A partial match such as inp= is left for R
  #to resolve against the formals, which still include the former names.
  used <- intersect(given, names(renamed))
  if (length(used) == 0) {
    return(NULL)
  }
  for (old in used) {
    .deprecatedArg(old, renamed[[old]], fn)
  }
  names(call)[match(used, given)] <- renamed[used]
  return(list(eval(call, env)))
}
