#' Install the BirdNET environment
#'
#' This function installs BirdNET in the `ssd_birdnet` environment using `reticulate`.
#'
#' @param unattended If TRUE then the function will not prompt the user to install
#'   the environment in a non-interactive session.
#' @export
#' @examples
#' \dontrun{
#' birdnetInstall()
#' birdNetInstall(unattended=TRUE)
#' }
birdNetInstall <- function(unattended=FALSE) {
  if (!package.installed("reticulate")) {
    stop("The reticulate package is required to create the birdnet environment.")
  }

  if (interactive()) {
    if (!utils::askYesNo("Do you want to install the birdnet environment?")) {
      stop("The birdnet environment is required to use the birdnetAnalyse function.")
    }
  } else {
    if (!unattended) {
      stop("The birdnet environment is required to use the birdnetAnalyse function.")
    }
  }

  if (!reticulate::virtualenv_exists(envname = "ssd_birdnet")) {
    reticulate::virtualenv_create(envname = "ssd_birdnet")
  }
  reticulate::virtualenv_install("ssd_birdnet", "librosa")
  reticulate::virtualenv_install("ssd_birdnet", "tensorflow")
  reticulate::virtualenv_install("ssd_birdnet", "birdnetlib")
}

birdNetAnalyse <- function() {
  reticulate::use_virtualenv("ssd_birdnet")
  bn <- reticulate::import("birdnetlib")
  bna <- reticulate::import("birdnetlib.analyzer")
  datetime <- reticulate::import("datetime")

  analyzer <- bna$Analyzer()

  recording <- bn$Recording(
    analyzer,
    system.file("extdata", "AUDIOMOTH.WAV", package = "sonicscrewdriver")
    )

  recording$analyze()
}
