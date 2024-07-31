#' Install the scikit-maad environment
#'
#' This function installs scikit-maad in the `ssd_scikit-maad` environment using `reticulate`.
#'
#' @param unattended If TRUE then the function will not prompt the user to install
#'   the environment in a non-interactive session.
#' @export
#' @examples
#' \dontrun{
#' maadInstall()
#' maadInstall(unattended=TRUE)
#' }
maadInstall <- function(unattended=FALSE) {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to create the `ssd_scikit-maad` environment.")
  }

  if (!unattended & interactive()) {
    if (!utils::askYesNo("Do you want to install the scikit-maad environment?")) {
      stop("The `ssd_scikit-maad` environment is required to use maad functions in sonicscrewdriver.")
    }
  } else {
    if (!unattended) {
      stop("The `ssd_scikit-maad` environment is required to use maad functions in sonicscrewdriver.")
    }
  }

  if (!reticulate::virtualenv_exists(envname = "ssd_scikit-maad")) {
    reticulate::virtualenv_create(envname = "ssd_scikit-maad", packages=NULL)
  }
  reticulate::virtualenv_install("ssd_scikit-maad", c("scikit-maad", "numpy==1.26.4"))
}

#' Import scikit-maad
#'
#' Imports the scikit-maad object into R. This allows reuse of the same object between
#' function calls.
#' @export
getMaad <- function() {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to use scikit-maad.")
  }
  if (!reticulate::virtualenv_exists(envname = "ssd_scikit-maad")) {
    maadInstall()
  }
  reticulate::use_virtualenv("ssd_scikit-maad")
  maad <- reticulate::import("maad")
  return(maad)
}

