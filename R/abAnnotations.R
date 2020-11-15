#' Get annotations from audioBlast
#'
#' Search for annotated audio sections on audioBlast (via the ann-o-mate project).
#'
#' @param ... Fields and values to filter on. Any field defined by the Ann-o-mate project
#' may be filtered. The most common is likely to be taxon.
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @return A data frame of matching annotations
#' @examples
#' \dontrun{
#' ab_annotations(taxon="Gryllotalpa vineae")
#' }
#'
ab_annotations <- function(...) {
  args <- list(...)
  nams <- names(args)
  url <- "https://api.audioblast.org/annotations?agent=sonicscrewdriver"
  for (i in 1:length(args)) {
    url <- paste0(url, "&")
    url <- paste0(url, nams[[i]], "=", args[[i]])
  }
  ret <- jsonlite::fromJSON(URLencode(url))
  return(ret)
}
