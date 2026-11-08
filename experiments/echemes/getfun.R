library(jsonlite)
get <- function(path, ...) {
  q <- list(...)
  q <- q[!vapply(q, is.null, logical(1))]
  u <- paste0("https://api.audioblast.org/data/", path, "/?",
              paste(sprintf("%s=%s", names(q), vapply(q, function(v)
                utils::URLencode(as.character(v), reserved=TRUE), character(1))),
                collapse="&"))
  out <- list(); page <- 1
  repeat {
    r <- tryCatch(fromJSON(paste0(u, "&page=", page)), error=function(e) NULL)
    if (is.null(r) || !is.data.frame(r$data) || nrow(r$data) == 0) break
    out[[length(out)+1]] <- r$data
    if (is.null(r$last_page) || page >= r$last_page) break
    page <- page + 1
  }
  if (!length(out)) return(NULL)
  do.call(rbind, lapply(out, function(d) d[, Reduce(intersect, lapply(out, names)), drop=FALSE]))
}
