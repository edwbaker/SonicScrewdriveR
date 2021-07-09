#' Nearest start time
#'
#' Search audioBLAST! for recordings with a start time closest to
#' specified date/time which match specified criteria
#'
#' @param ... Fields and values to filter on.
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @return A data frame of matching annotations
#' @examples
#' \dontrun{
#' ab_seqss_nearestStart(date="2020-05-15",time="1500")
#' }
#'
ab_seqss_nearestStart <- function(...) {
  .Deprecated("audioblast")
  args <- list(...)
  nams <- names(args)
  url <- "https://api.audioblast.org/standalone/seqss/recording_nearest_start_time/?output=nakedJSON"
  for (i in 1:length(args)) {
    url <- paste0(url, "&")
    url <- paste0(url, nams[[i]], "=", args[[i]])
  }
  ret <- jsonlite::fromJSON(URLencode(url))
  return(ret)
}
