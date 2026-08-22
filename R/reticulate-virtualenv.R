#' Install sonicscrewdriver Python environment
#'
#' This function installs scikit-maad in the `sonicscrewdriver` environment using `reticulate`.
#'
#' @param unattended If TRUE then the function will not prompt the user to install
#'   the environment in a non-interactive session.
#' @return No return value, called for its side effect of creating the Python environment.
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

  .useUtf8Locale()
  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    reticulate::virtualenv_create(envname = "sonicscrewdriver", packages=NULL, python_version="3.12")
  }
  reticulate::virtualenv_install("sonicscrewdriver", c("scikit-maad", "numpy==1.26.4", "librosa", "tensorflow", "birdnetlib"))
}

#' Name an available UTF-8 locale
#'
#' Which UTF-8 locales exist differs between systems: C.UTF-8 is present on
#' Linux but not on macOS, which has UTF-8 instead. Each candidate is therefore
#' tried in turn and the first one the system accepts is used. The language is
#' taken from the locale R is already in, so that it is not changed along with
#' the encoding. Python overwrites LC_CTYPE alone as it starts, so LC_COLLATE
#' still carries the language where LC_CTYPE has been reduced to C.
#'
#' @return The name of a UTF-8 locale, or NULL if the system accepts none of the
#'   candidates.
#' @keywords internal
#' @noRd
.utf8LocaleName <- function() {
  languages <- sub("\\..*$", "", c(Sys.getlocale("LC_CTYPE"), Sys.getlocale("LC_COLLATE")))
  languages <- languages[nzchar(languages) & !languages %in% c("C", "POSIX")]
  candidates <- unique(c(paste0(languages, ".UTF-8"), "C.UTF-8", "en_US.UTF-8", "UTF-8"))

  original <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", original)), add=TRUE)
  for (candidate in candidates) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", candidate)))) {
      return(candidate)
    }
  }
  return(NULL)
}

#' Put the process in a UTF-8 locale for Python
#'
#' Python reads the locale of the process as it initialises and takes from it
#' the default encoding for `open()`. Where the environment names no locale it
#' settles on C, and so on ASCII, and reading a file holding anything outside
#' ASCII then raises a UnicodeDecodeError. The species labels shipped with
#' `birdnetlib` are such a file, so `birdNetAnalyse()` fails on the accented
#' names in it. R takes its own locale from the system settings without putting
#' it in the environment, which is why R is unaffected where Python is not, and
#' why this is only seen where nothing else sets a locale: cron jobs,
#' containers, continuous integration and `R CMD check`.
#'
#' Both the environment variable and the locale itself are set. The variable is
#' what Python reads if it has yet to start, and setting it first also spares R
#' the locale Python would otherwise impose, as Python overwrites the LC_CTYPE
#' of the process with its own choice on the way up. Setting the locale repairs
#' matters where Python has already started, the encoding being looked up afresh
#' at each `open()`.
#'
#' LC_CTYPE alone is set, as it governs the encoding and neither the language of
#' messages nor the formatting of numbers, and only where none of LC_ALL,
#' LC_CTYPE or LANG is set already. A locale the user has chosen is left as it
#' is.
#'
#' @return Invisibly, the locale set, or NULL if none was.
#' @keywords internal
#' @noRd
.useUtf8Locale <- function() {
  if (any(nzchar(Sys.getenv(c("LC_ALL", "LC_CTYPE", "LANG"))))) {
    return(invisible(NULL))
  }
  target <- .utf8LocaleName()
  if (is.null(target)) {
    return(invisible(NULL))
  }
  Sys.setenv(LC_CTYPE=target)
  suppressWarnings(Sys.setlocale("LC_CTYPE", target))
  return(invisible(target))
}
