#' Import scikit-maad
#'
#' Imports the scikit-maad object into R. This allows reuse of the same object between
#' function calls.
#' @return The scikit-maad Python module, with its sound and features submodules attached.
#' @export
getMaad <- function() {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to use scikit-maad.")
  }
  .useUtf8Locale()
  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    pythonInstall()
  }
  reticulate::use_virtualenv("sonicscrewdriver")
  maad <- reticulate::import("maad")
  maad$sound <- reticulate::import("maad.sound")
  maad$features <- reticulate::import("maad.features")
  return(maad)
}

#' Resolve an optional maad argument
#'
#' The scikit-maad wrappers all take an optional `maad`, so that a caller
#' analysing many windows can import the module once. Each of them repeated the
#' same three-line guard.
#'
#' @param maad A scikit-maad module object, or NULL.
#' @return The module given, or a newly imported one.
#' @noRd
.maad <- function(maad=NULL) {
  if (is.null(maad)) {
    return(getMaad())
  }
  return(maad)
}
