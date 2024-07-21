#' Install the scikit-maad environment
#'
#' This function installs scikit-maad in the `ssd_scikit-maad` environment using `reticulate`.
#'
#' @param unattended If TRUE then the function will not prompt the user to install
#'   the environment in a non-interactive session.
#' @export
#' @examples
#' \dontrun{
#' birdnetInstall()
#' birdNetInstall(unattended=TRUE)
#' }
maadInstall <- function(unattended=FALSE) {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to create the `ssd_birdnet` environment.")
  }

  if (interactive()) {
    if (!utils::askYesNo("Do you want to install the scikit-maad environment?")) {
      stop("The `ssd_scikit-maad` environment is required to use maad functions in sonicscrewdriver.")
    }
  } else {
    if (!unattended) {
      stop("The `ssd_scikit-maad` environment is required to use maad functions in sonicscrewdriver.")
    }
  }

  if (!reticulate::virtualenv_exists(envname = "ssd_scikit-maad")) {
    reticulate::virtualenv_create(envname = "ssd_scikit-maad")
  }
  reticulate::virtualenv_install("ssd_scikit-maad", "scikit-maad")
}
