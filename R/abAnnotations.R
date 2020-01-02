#' Get annotations from audioBlast
#'
#' Search for annotated audio sections on audioBlast (via the ann-o-mate project).
#'
#' @param ... Fields and values to filter on.
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @return A data frame of matching annotations
#' @examples
#' audioblast_annotations(taxon="Gryllotalpa vineae")
audioblast_annotations <- function(...) {
  args <- list(...)
  nams <- names(args)
  url <- "https://api.audioblast.org/annotations?"
  for (i in 1:length(args)) {
    if (i > 1) {url <- paste0(url, "&")}
    url <- paste0(url, nams[[i]], "=", args[[i]])
  }
  ret <- jsonlite::fromJSON(URLencode(url))
  return(ret)
}
