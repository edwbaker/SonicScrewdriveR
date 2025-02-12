#' Import scikit-maad
#'
#' Imports the scikit-maad object into R. This allows reuse of the same object between
#' function calls.
#' @export
getMaad <- function() {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to use scikit-maad.")
  }
  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    pythonInstall()
  }
  reticulate::use_virtualenv("sonicscrewdriver")
  maad <- reticulate::import("maad")
  maad$sound <- reticulate::import("maad.sound")
  maad$features <- reticulate::import("maad.features")
  return(maad)
}

