#' Get data or analyses from audioBlast
#'
#' Search for recordings or analyses on audioBlast.
#'
#' @param type One of data, analysis, standalone.
#' @param name of data or analysis source.
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
audioblast <- function(type, name, ...) {
  args <- list(...)
  nams <- names(args)
  url <- paste0("https://api.audioblast.org/",type,"/",name,"/?agent=sonicscrewdriver")
  for (i in 1:length(args)) {
    url <- paste0(url, "&", nams[[i]], "=", args[[i]])
  }
  res <- jsonlite::fromJSON(URLencode(url))
  ret <- res$data
  attr(ret, "notes") <- res$notes
  return(ret)
}
