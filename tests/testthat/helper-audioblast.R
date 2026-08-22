# api.audioblast.org answers R's default user agent with a 403, so the tests
# that need real data cannot run everywhere. The probe goes through the same
# connection the package itself uses, rather than an external tool, so that it
# fails in exactly the cases the tests would.
.audioblastReachable <- local({
  known <- NULL
  function() {
    if (is.null(known)) {
      old <- options(timeout=15)
      on.exit(options(old), add=TRUE)
      known <<- tryCatch({
        res <- suppressWarnings(jsonlite::fromJSON(
          "https://api.audioblast.org/standalone/modules/module_info/?module=recordings"
        ))
        #A Cloudflare challenge can return successfully without holding the
        #module description the tests go on to ask for.
        "mname" %in% names(res$data)
      }, error=function(e) FALSE)
    }
    return(known)
  }
})

skip_if_no_audioblast <- function() {
  testthat::skip_if_not(.audioblastReachable(), "the audioBlast API is not reachable")
}
