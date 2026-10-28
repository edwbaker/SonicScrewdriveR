# The tests of audioblast() read api.audioblast.org where they can, and fall back
# to responses saved from it where they cannot. CI cannot always reach the API:
# it has timed out there, and it answers R's default user agent with a 403 in
# some places. Once one read fails, every later one in the run uses the saved
# responses, so that a run does not wait out a timeout for each request. On CRAN
# nothing is read from the network at all.
#
# The saved responses are in fixtures/audioblast, each under a name made from
# the URL it answers. Running these tests with AUDIOBLAST_RECORD=true, where the
# API can be reached, saves what it answers in their place.
.audioblastState <- new.env(parent=emptyenv())

#' Whether the network is still to be tried
#'
#' Decided at the first request rather than when this file is read, as
#' `pkgload::load_all()` reads the helpers with NOT_CRAN set to true whatever
#' the tests then run with.
.audioblastLive <- function() {
  if (is.null(.audioblastState$live)) {
    .audioblastState$live <- isTRUE(as.logical(Sys.getenv("NOT_CRAN")))
  }
  return(.audioblastState$live)
}

#' The file holding the saved response to a request
#'
#' Named from the start of the request, so that it can be found by eye, and
#' from a hash of the whole of it, so that two requests with the same start do
#' not share a file. A whole request made into a name is too long for the paths
#' a package tarball can hold.
.audioblastFixture <- function(url) {
  start <- sub("^https://api\\.audioblast\\.org/", "", url)
  start <- sub("-+$", "", substr(gsub("[^A-Za-z0-9]+", "-", start), 1L, 24L))
  f <- tempfile()
  on.exit(unlink(f), add=TRUE)
  writeBin(charToRaw(url), f)
  hash <- substr(unname(tools::md5sum(f)), 1L, 8L)
  return(testthat::test_path("fixtures", "audioblast", paste0(start, "-", hash, ".json")))
}

#' Note that the network could not be used, and use the saved responses from now on
.audioblastOffline <- function(url, cond) {
  if (.audioblastLive()) {
    .audioblastState$live <- FALSE
    # Written to stderr so that a test expecting silence is not disturbed.
    cat(
      "audioBlast could not be read at ", url, " (", conditionMessage(cond),
      "), so the audioblast tests use saved responses from here on.\n",
      file=stderr(), sep=""
    )
  }
}

#' Read a request from audioBlast, or from its saved response
.audioblastRead <- function(url, fromJSON, ...) {
  path <- .audioblastFixture(url)
  if (.audioblastLive()) {
    old <- options(timeout=15)
    on.exit(options(old), add=TRUE)
    if (identical(Sys.getenv("AUDIOBLAST_RECORD"), "true")) {
      dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
      utils::download.file(url, path, quiet=TRUE, mode="wb")
      return(fromJSON(path, ...))
    }
    res <- tryCatch({
      res <- fromJSON(url, ...)
      # A Cloudflare challenge can come back as a response that holds nothing
      # the API would have sent. The API echoes the parameters it was given in
      # every answer, even one with no data, such as for a module that does not
      # exist.
      if (!("params" %in% names(res))) {
        stop("the response is not one the API sends")
      }
      res
    }, error=function(e) e, warning=function(w) w)
    if (!inherits(res, "condition")) {
      return(res)
    }
    .audioblastOffline(url, res)
  }
  if (!file.exists(path)) {
    stop(
      "There is no saved audioBlast response for ", url, ". Run the tests with ",
      "AUDIOBLAST_RECORD=true where the API can be reached to save one.",
      call.=FALSE
    )
  }
  return(fromJSON(path, ...))
}

#' Answer the audioBlast requests of a test live where possible, or from saved responses
local_audioblast <- function(env=parent.frame()) {
  fromJSON <- jsonlite::fromJSON
  testthat::local_mocked_bindings(
    fromJSON = function(txt, ...) .audioblastRead(txt, fromJSON, ...),
    .package = "jsonlite",
    .env = env
  )
}

#' Download a file where possible, or write a stand-in for it
#'
#' The file is a recording from the site audioBlast indexes, which the tests
#' only need to see written where it was asked for.
.audioblastDownload <- function(url, destfile, quiet=FALSE, ...) {
  if (.audioblastLive()) {
    old <- options(timeout=15)
    on.exit(options(old), add=TRUE)
    done <- tryCatch({
      utils::download.file(url, destfile, quiet=quiet, mode="wb", ...)
      TRUE
    }, error=function(e) e, warning=function(w) w)
    if (isTRUE(done)) {
      return(invisible(0L))
    }
    .audioblastOffline(url, done)
  }
  writeBin(charToRaw("RIFF"), destfile)
  return(invisible(0L))
}
