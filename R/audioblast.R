#' Get data or analyses from audioBlast
#'
#' Search for recordings or analyses on audioBlast.
#'
#' @param type One of data, analysis, standalone.
#' @param name Name of data or analysis source.
#' @param endpoint Optionally specify endpoint of an audioBlast module.
#' @param ... Fields and values to filter on. Any field defined by audioBLAST.
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @return A data frame
#' @examples
#' \dontrun{
#' audioblast("data", "recordings", taxon="Gryllotalpa vineae")
#' }
#'
audioblast <- function(type, name, endpoint=NULL, ...) {
  args <- list(...)
  nams <- names(args)
  url <- paste0("https://api.audioblast.org/",type,"/",name,"/")
  if (!is.null(endpoint)) {
    url <- paste0(url, endpoint, "/")
  }
  url <- paste0(url, "?agent=sonicscrewdriver")
  for (i in 1:length(args)) {
    url <- paste0(url, "&", nams[[i]], "=", args[[i]])
  }
  res <- jsonlite::fromJSON(URLencode(url))
  ret <- res$data
  attr(ret, "notes") <- res$notes
  return(ret)
}
