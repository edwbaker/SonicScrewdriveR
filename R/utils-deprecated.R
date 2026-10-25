#' Warn that an argument has been renamed
#'
#' The unit arguments were named input, unit or units depending on which function
#' they belonged to. They are now input.unit for the unit a value is supplied in
#' and output.unit for the unit a result is returned in. The former names still
#' work, through a shim in each function that calls this.
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
