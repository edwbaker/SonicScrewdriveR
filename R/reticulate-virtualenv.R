#' Install sonicscrewdriver Python environment
#'
#' This function installs scikit-maad in the `sonicscrewdriver` environment using `reticulate`.
#'
#' @param unattended If TRUE then the function will not prompt the user to install
#'   the environment in a non-interactive session.
#' @export
#' @examples
#' \dontrun{
#' pythonInstall()
#' pythonInstall(unattended=TRUE)
#' }
pythonInstall <- function(unattended=FALSE) {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to create the `sonicscrewdriver` environment.")
  }

  if (!unattended & interactive()) {
    if (!utils::askYesNo("Do you want to install the sonicscrewdriver environment?")) {
      stop("The `sonicscrewdriver` environment is required to use maad functions in sonicscrewdriver.")
    }
  } else {
    if (!unattended) {
      stop("The `sonicscrewdriver` environment is required to use maad functions in sonicscrewdriver.")
    }
  }

  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    reticulate::virtualenv_create(envname = "sonicscrewdriver", packages=NULL)
  }
  reticulate::virtualenv_install("sonicscrewdriver", c("scikit-maad==1.4.3", "numpy==1.26.4", "librosa", "tensorflow", "birdnetlib"))
}
