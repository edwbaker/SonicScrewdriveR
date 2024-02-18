#' Get data or analyses from audioBlast
#'
#' Search for data or analyses on audioBlast.
#'
#' @param type One of data, analysis, standalone.
#' @param name Name of data or analysis source.
#' @param endpoint Optionally specify endpoint of an audioBlast module.
#' @param check Logical. Performs sanity check on input before sending to audioBLAST.
#' @param page First page of results to request, defaults to 1.
#' @param max_pages Maximum number of data pages to return, by default this is set to NULL and returns all pages.
#' @param quiet If true will not print progress. Silence is a virtue.
#' @param ... Fields and values to filter on. Any field defined by audioBLAST.
#' @param output By default a `data.frame`. "Annotations" will return a list of
#'   `Annotation` objects.
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return A data frame
#' @examples
#' \dontrun{
#' audioblast("data", "recordings", taxon="Gryllotalpa vineae")
#' }
#'
audioblast <- function(
    type,
    name,
    endpoint=NULL,
    check=TRUE,
    max_pages=NULL,
    page=1,
    quiet=FALSE,
    output="data.frame",
    ...
) {
  if (!output %in% c("data.frame", "Annotations")) {
    stop(paste(output, "is not a valid output type."))
  }
  if (output=="Annotations") {
    if (type != "data" | name != "annomate") {
      stop("Query does not gives results that can be turned into Annotation objects.")
    }
  }
  if (!is.null(max_pages)) {
    if (max_pages == 1) {
      quiet <- TRUE
    }
  }
  args <- list(...)
  nams <- names(args)
  if (check) {
    c <- .audioblast_ASITSN(type, name, endpoint)
  }
  url <- paste0("https://api.audioblast.org/",type,"/",name,"/")
  if (!is.null(endpoint)) {
    url <- paste0(url, endpoint, "/")
  }
  if (type=="standalone") {
    max_pages <- 1
    if (length(args) > 0) {
      for (i in 1:length(args)) {
        if (i==1) {
          url <- paste0(url, "?")
        } else {
          url <- paste0(url, "&")
        }
        url <- paste0(url, nams[[i]], "=", args[[i]])
      }
    }
  } else {
    url <- paste0(url, "?page=", page)
    if (length(args) > 0) {
      for (i in 1:length(args)) {
        url <- paste0(url, "&", nams[[i]], "=", args[[i]])
      }
    }
  }
  res <- jsonlite::fromJSON(URLencode(url))
  if (is.null(res$data)) {
    return(NULL)
  }
  ret <- res$data
  mp <- min(res$last_page, max_pages)
  page <- page + 1
  if (!quiet) {
    pb <- txtProgressBar(min = 0, max = mp, initial = page)
  }
  while (page <= mp) {
    url <- paste0("https://api.audioblast.org/",type,"/",name,"/")
    if (!is.null(endpoint)) {
      url <- paste0(url, endpoint, "/")
    }
    url <- paste0(url, "?page=", page)
    if (length(args) > 0) {
      for (i in 1:length(args)) {
        url <- paste0(url, "&", nams[[i]], "=", args[[i]])
      }
    }
    res <- jsonlite::fromJSON(URLencode(url))
    ret <- rbind(ret, res$data)
    page <- page + 1
    if (!quiet) {
      setTxtProgressBar(pb,page)
    }
  }
  if (!quiet) {
    close(pb)
  }

  if (output=="Annotations") {
    l <- vector(mode="list", length=nrow(ret))
    for (i in 1:nrow(ret)) {
      l[[i]] <- annotation(
        metadata = list(
          "source" = ret[i, "source"],
          "source_id" = ret[i, "source_id"],
          "annotator" = ret[i, "annotator"],
          "annotation_id" = ret[i, "annotation_id"],
          "annotation_date" = ret[i, "annotation_date"],
          "annotation_info_url" = ret[i, "annotation_info_url"],
          "lat" = ret[i, "lat"],
          "lon" = ret[i, "lon"],
          "contact" = ret[i, "contact"]
        ),
        file = ret[i, "recording_url"],
        source = "audioblast",
        start = ret[i, "time_start"],
        end = ret[i, "time_end"],
        type = ret[i, "type"],
        value = ret[i, "taxon"]
      )
    }
    ret <- l
  }
  return(ret)
}

#' audioBlast - a stitch in time saves nine
#'
#' Sanity check before sending request to audioBlast
#'
#' @param type One of data, analysis, standalone.
#' @param name Name of data or analysis source.
#' @param endpoint Optionally specify endpoint of an audioBlast module.
#' @param ... Fields and values to filter on. Any field defined by audioBLAST.
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
.audioblast_ASITSN <- function(type, name, endpoint=NULL, ...) {
  args <- list(...)
  nams <- names(args)
  url <- paste0("https://api.audioblast.org/standalone/modules/module_info/?module=",name)
  res <- jsonlite::fromJSON(URLencode(url))
  if (!("mname" %in% names(res$data))) {
    stop(paste(name, "module does not exist."))
  }

  if (!is.null(endpoint)) {
    if (!(endpoint %in% names(res$data$endpoints))) {
      stop(paste(endpoint, "is not a valid endpoint."))
    }
  }
}

#' Download audio files from audioBlast
#'
#' Downloads audio files associated with a search using the audioBlast() function.
#'
#' @param d Data returned from a search using audioBlast().
#' @param metadata If true saves the data in d as a csv file.
#' @param skip.existing If true will not overwrite existing files.
#' @param dir Directory to save files to.
#' @param quiet If true will not print progress.
#' @export
#' @importFrom utils download.file write.csv
audioblastDownload <- function(d, metadata=TRUE, skip.existing=TRUE, dir=".", quiet=FALSE) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  if (metadata) {
    write.csv(d, file=paste(dir, "metadata.csv", sep="/"))
  }
  files <- d[, 'filename']
  names <- basename(files)
  if (skip.existing) {
    files <- files[file.exists(names)==FALSE]
    names <- names[file.exists(names)==FALSE]
  }
  for (i in 1:length(files)) {
    download.file(files[i], destfile=paste(dir, names[i], sep="/"), quiet=quiet)
  }
}

