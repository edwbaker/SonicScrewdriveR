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
#
# The requests are made with httr2, so this stands in at the point httr2
# performs one. A mock that returns NULL lets the request happen for real, and
# a request can be performed for real from inside a mock by setting the mock
# aside, which is how a live read is tried before a saved one is used. A mocked
# response does not write a file, so a download of a recording is stood in for
# separately, at the function that performs it.
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

#' Perform a request for real, with the mock set aside
#'
#' Called from inside the mock, so the mock has to be cleared for the length of
#' the call or it would answer its own request.
.audioblastPerformReal <- function(req) {
  httr2::with_mocked_responses(NULL, httr2::req_perform(req))
}

#' A response holding a saved body, as though the API had sent it
.audioblastSaved <- function(path) {
  httr2::response(
    status_code=200,
    headers=list("Content-Type"="application/json"),
    body=readBin(path, "raw", file.size(path))
  )
}

#' Answer one request from audioBlast, or from its saved response
.audioblastRespond <- function(req) {
  path <- .audioblastFixture(req$url)
  if (.audioblastLive()) {
    old <- options(timeout=15)
    on.exit(options(old), add=TRUE)
    if (identical(Sys.getenv("AUDIOBLAST_RECORD"), "true")) {
      resp <- .audioblastPerformReal(req)
      dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
      writeBin(httr2::resp_body_raw(resp), path)
      return(resp)
    }
    res <- tryCatch({
      resp <- .audioblastPerformReal(req)
      # A Cloudflare challenge can come back as a response that holds nothing
      # the API would have sent, and it is a page of HTML where every answer the
      # API gives is JSON or RDF. Looking for the parameters it echoes would
      # reject the answers that carry none, such as a single record or Turtle.
      if (grepl("^\\s*<", httr2::resp_body_string(resp))) {
        stop("the response is a page rather than one the API sends")
      }
      resp
    }, error=function(e) e, warning=function(w) w)
    if (!inherits(res, "condition")) {
      return(res)
    }
    .audioblastOffline(req$url, res)
  }
  if (!file.exists(path)) {
    stop(
      "There is no saved audioBlast response for ", req$url, ". Run the tests with ",
      "AUDIOBLAST_RECORD=true where the API can be reached to save one.",
      call.=FALSE
    )
  }
  return(.audioblastSaved(path))
}

#' Write a stand-in for a recording, or download it where that is possible
#'
#' The file is a recording from the site audioBlast indexes, which the tests
#' only need to see written where it was asked for. A mocked response does not
#' write a file, so this stands in for the download itself rather than for the
#' response to it.
.audioblastDownload <- function(url, path) {
  if (.audioblastLive()) {
    done <- tryCatch({
      .audioblastPerformReal(
        httr2::req_retry(httr2::request(url), max_tries=2, retry_on_failure=TRUE)
      )
    }, error=function(e) e, warning=function(w) w)
    if (!inherits(done, "condition")) {
      writeBin(httr2::resp_body_raw(done), path)
      return(invisible(path))
    }
    .audioblastOffline(url, done)
  }
  writeBin(charToRaw("RIFF"), path)
  return(invisible(path))
}

#' Answer the audioBlast requests of a test live where possible, or from saved responses
local_audioblast <- function(env=parent.frame()) {
  httr2::local_mocked_responses(.audioblastRespond, env=env)
  testthat::local_mocked_bindings(
    .audioblast_download = .audioblastDownload,
    .env = env
  )
}
