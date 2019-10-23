#' @importFrom utils askYesNo install.packages installed.packages
package.installed <- function(name, method=NULL, repo=NULL) {
  if(name %in% rownames(installed.packages()) == TRUE) {return(TRUE)}
  else {
    prompt <- askYesNo(paste("Operation requires missing package ",name,". Install now?"))
    if (prompt == FALSE | is.na(prompt)) {
      stop("Missing package not installed.")
    } else {
      if (is.null(method)) {
        install.packages(name)
        return(TRUE)
      }
      if (method == "github") {
        if (package.installed("devtools")) {
          devtools::install_github(paste(repo,name, sep = "/"))
          return(TRUE)
        }
      }
    }
  }
}