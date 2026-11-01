#' Whether a package can be used, offering to install it where it cannot
#'
#' The package's one guard for an optional dependency. TRUE where the package
#' is there to be used, FALSE where it is not and nothing was installed, and an
#' error only where someone was asked and said no: a caller that is told FALSE
#' says for itself which package it wanted and what it wanted it for.
#'
#' @param name The package wanted.
#' @param method NULL to install from the repositories, or "github".
#' @param repo The GitHub user or organisation, where method is "github".
#' @param askInstall Whether to offer to install the package where it is
#'   missing. An offer needs someone to make it to, so nothing is installed in
#'   a session with no one to ask whatever this says.
#' @keywords internal
#' @noRd
#' @importFrom utils askYesNo install.packages
package.installed <- function(name, method=NULL, repo=NULL, askInstall=TRUE) {
  if (.packageUsable(name)) {
    return(TRUE)
  }
  #Nothing answers askYesNo() where there is no one to ask, and it returns its
  #default, which is TRUE: a non-interactive session went straight on to
  #install.packages() and wrote into the library unasked, so R CMD check, CI
  #and cron runs all attempted installs. A plotting function has no business
  #installing a package as a side effect of drawing, and nothing here can ask
  #first, so the package is reported missing instead and the caller says what
  #it was for.
  if (!askInstall || !.canAsk()) {
    return(FALSE)
  }
  prompt <- askYesNo(paste0("Operation requires missing package ", name, ". Install now?"))
  #askYesNo() returns NA where the prompt is cancelled.
  if (isFALSE(prompt) || is.na(prompt)) {
    stop("Missing package not installed.")
  }
  if (is.null(method)) {
    install.packages(name)
  } else if (method == "github") {
    if (!package.installed("devtools")) {
      return(FALSE)
    }
    devtools::install_github(paste(repo, name, sep = "/"))
  } else {
    #Anything else installed nothing and fell off the end of the function
    #returning NULL, which is neither TRUE nor FALSE: a caller testing it with
    #if () failed with "argument is of length zero".
    stop(paste0("Unknown method for installing a package: ", method))
  }
  #install.packages() returns nothing whether or not it installed anything, and
  #reports a failure it noticed as a warning, so TRUE was returned for an
  #install that had not happened - no network, or a library that cannot be
  #written - and the caller went on to use a package that was not there. What
  #was asked for is looked for again instead of taking the installer's word.
  return(.packageUsable(name))
}

#' Whether a package is there to be used
#'
#' Asked of the loader rather than of the library: installed.packages() lists
#' what is in the library, which is not the same question as whether it can be
#' used, and is also what has to be asked after an install to find out whether
#' the install produced anything. Wrapped so that a test can stand in for it,
#' as a base function cannot be stood in for.
#'
#' @param name The package wanted.
#' @return TRUE where the package's namespace loads.
#' @keywords internal
#' @noRd
.packageUsable <- function(name) {
  return(requireNamespace(name, quietly=TRUE))
}

#' Whether there is anyone to ask
#'
#' Wrapped for the same reason as .packageUsable().
#'
#' @return TRUE in a session that can be prompted.
#' @keywords internal
#' @noRd
.canAsk <- function() {
  return(interactive())
}
