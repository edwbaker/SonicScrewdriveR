#' Get data or analyses from audioBlast
#'
#' Search for data or analyses on [audioBlast](https://audioblast.org).
#'
#' Filters are given as named arguments in `...`. audioBlast refuses a filter
#' that a module does not have with an error naming it, and
#' [audioblastFilters()] lists the filters a module has.
#'
#' A filter that audioBlast lists as taking several values can be given a
#' vector, e.g. `id = c(10754, 11096)`, which matches a row holding any one of
#' them. audioBlast takes at most 100 values in a request, so longer vectors are
#' sent as several requests and their results combined.
#'
#' A filter on a range (e.g. `duration` or `lat`) can be given a single value,
#' matched exactly, or a range made with [ab_range()], e.g.
#' `duration = ab_range(10, 20)`.
#'
#' Columns that audioBlast describes as numeric are returned as numbers, and
#' all others as character strings.
#'
#' @param type One of data, analysis, standalone.
#' @param name Name of data or analysis source.
#' @param endpoint Optionally specify endpoint of an audioBlast module.
#' @param check Logical. Checks that the module and endpoint exist before
#'   sending the query to audioBlast.
#' @param max_pages Maximum number of data pages to return, by default this is
#'   set to NULL and returns all pages.
#' @param page First page of results to request, defaults to 1.
#' @param page_size Number of results on each page, defaults to 50. Larger pages
#'   need fewer requests to fetch many results.
#' @param quiet If true will not print progress. Silence is a virtue.
#' @param on.issue Function to call on error or warning. By default `stop` to raise
#'   a standard R error. Setting to `warning` will instead a warning.
#' @param output By default a `data.frame`. "Annotations" will return a list of
#'   `Annotation` objects. "JSON-LD" and "Turtle" return the RDF that audioBlast
#'   gives for modules that describe their records as linked data, as a
#'   character vector with one element for each page.
#' @param ... Fields and values to filter on. Any field defined by audioBlast.
#' @export
#' @return A data frame, a list of `Annotation` objects or a character vector,
#'   depending on `output`. `NULL` if nothing matches the query.
#' @seealso [audioblastFilters()] for the filters a module takes,
#'   [audioblastRecord()] to fetch one record, and [audioblastDownload()] to
#'   download the recordings found.
#' @examples
#' \dontrun{
#' audioblast("data", "recordings", taxon="Gryllotalpa vineae")
#'
#' # Several recordings at once
#' audioblast("data", "recordings", source="bio.acousti.ca", id=c(10754, 11096))
#'
#' # Recordings between 10 and 20 seconds long
#' audioblast("data", "recordings", source="bio.acousti.ca",
#'            duration=ab_range(10, 20), max_pages=1)
#'
#' # The classification of a taxon, from the root down
#' audioblast("data", "taxa", "classification", source="bio.acousti.ca", id=247)
#' }
#'
audioblast <- function(
    type,
    name,
    endpoint=NULL,
    check=TRUE,
    max_pages=NULL,
    page=1,
    page_size=50,
    quiet=FALSE,
    on.issue = stop,
    output="data.frame",
    ...
) {
  .validateChoice(
    output, c("data.frame", "Annotations", "JSON-LD", "Turtle"),
    msg=paste(output, "is not a valid output type.")
  )
  if (!is.numeric(page_size) || length(page_size) != 1 || page_size < 1) {
    stop("page_size must be a number of at least 1.")
  }
  if (!is.null(max_pages) && (!is.numeric(max_pages) || max_pages < 1)) {
    stop("max_pages must be NULL or a number of at least 1.")
  }
  if (output=="Annotations") {
    if (type != "data" | name != "annomate") {
      stop("Query does not gives results that can be turned into Annotation objects.")
    }
  }
  filters <- .audioblast_filters(list(...))

  tryCatch({
    info <- NULL
    if (check) {
      info <- .audioblast_ASITSN(type, name, endpoint)
    }
    path <- paste(c(type, name, endpoint), collapse="/")

    # Standalone modules and module endpoints answer in one response, without pages
    if (type == "standalone" | !is.null(endpoint)) {
      if (output %in% c("JSON-LD", "Turtle")) {
        stop(output, " output is only available from searches of a module.")
      }
      res <- .audioblast_json(.audioblast_perform(path, filters))
      ret <- res$data
      if (length(ret) == 0) {
        return(NULL)
      }
      if (is.data.frame(ret)) {
        ret <- .audioblast_convert(ret, info)
      }
      return(ret)
    }

    # A filter given more than 100 values is sent in batches of 100
    batches <- .audioblast_batches(filters)
    if (length(batches) > 1) {
      rets <- lapply(batches, function(b) {
        .audioblast_pages(path, b, page, page_size, max_pages, TRUE, output)
      })
    } else {
      rets <- list(.audioblast_pages(path, filters, page, page_size, max_pages, quiet, output))
    }

    if (output %in% c("JSON-LD", "Turtle")) {
      return(unlist(rets))
    }
    rets <- rets[!vapply(rets, is.null, logical(1))]
    if (length(rets) == 0) {
      return(NULL)
    }
    ret <- .audioblast_convert(do.call(rbind, rets), info)

    if (output=="Annotations") {
      ret <- .audioblast_annotations(ret)
    }
    return(ret)
  }, error=function(e) .audioblast_issue(e, on.issue), warning=function(w) .audioblast_issue(w, on.issue))
}

#' Get one record from audioBlast
#'
#' Each record held by audioBlast has its own address,
#' `https://api.audioblast.org/{type}/{source}/{id}`, which gives the record
#' as data or as RDF. As RDF, a record at its own address carries more than the
#' same record found by [audioblast()], such as the records linked to it.
#'
#' @param type The type of the record, one of "recording", "annotation",
#'   "taxon", "trait", "image", "specimen", "location", "description",
#'   "vernacular-name", "onomatopoeia", "reference" or "link".
#' @param source The source holding the record.
#' @param id The ID of the record within its source. For an annotation this is
#'   its annotation ID.
#' @param output By default a `data.frame`. "JSON-LD" and "Turtle" return the
#'   record as RDF, in a character string.
#' @return A data frame with one row, or a character string, depending on
#'   `output`. `NULL` if the source holds no such record.
#' @export
#' @examples
#' \dontrun{
#' audioblastRecord("recording", "bio.acousti.ca", 10754)
#' audioblastRecord("taxon", "bio.acousti.ca", 247, output="Turtle")
#' }
audioblastRecord <- function(type, source, id, output="data.frame") {
  type <- match.arg(type, .audioblast_record_types)
  output <- match.arg(output, c("data.frame", "JSON-LD", "Turtle"))
  # An ID can contain a slash, which audioBlast reads as part of it
  escape <- function(x) gsub("%2F", "/", utils::URLencode(as.character(x), reserved=TRUE), fixed=TRUE)
  path <- paste(type, escape(source), escape(id), sep="/")
  query <- list()
  if (output != "data.frame") {
    query$output <- output
  }
  resp <- .audioblast_perform(path, query, allow_404=TRUE, slash=FALSE)
  if (httr2::resp_status(resp) == 404) {
    return(NULL)
  }
  if (output != "data.frame") {
    return(httr2::resp_body_string(resp))
  }
  ret <- .audioblast_json(resp)$data
  if (length(ret) == 0) {
    return(NULL)
  }
  module <- names(.audioblast_record_types)[.audioblast_record_types == type]
  return(.audioblast_convert(ret, .audioblast_module_info(module)))
}

#' List the filters of an audioBlast module
#'
#' audioBlast refuses a query that filters a module on a field it does not have,
#' so this function lists the fields that can be used as filters in
#' [audioblast()].
#'
#' @param name Name of an audioBlast module, e.g. "recordings" or "aci".
#' @return A data frame with one row for each filter: its `name`, a
#'   `description`, its `type`, whether it takes several values at once
#'   (`multiple`) and whether it matches values that contain it (`contains`),
#'   rather than only values equal to it.
#' @export
#' @examples
#' \dontrun{
#' audioblastFilters("recordings")
#' }
audioblastFilters <- function(name) {
  info <- .audioblast_module_info(name)
  if (is.null(info)) {
    stop(paste(name, "module does not exist."))
  }
  params <- info$params
  params <- params[vapply(params, function(p) !is.null(p$op) && p$op %in% c("=", "contains", "range"), logical(1))]
  data.frame(
    name = names(params),
    description = vapply(params, function(p) .audioblast_null(p$desc, ""), character(1)),
    type = vapply(params, function(p) .audioblast_null(p$type, ""), character(1)),
    multiple = vapply(params, function(p) isTRUE(p$multiple), logical(1)),
    contains = vapply(params, function(p) p$op == "contains", logical(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Make a range to filter audioBlast on
#'
#' Filters on a range, such as the duration of a recording or its latitude, can
#' be given a range made by this function. Both ends of the range are included
#' in it. Leaving out one end gives a range open at that end.
#'
#' @param min Lowest value of the range, or `NULL` for no lower limit.
#' @param max Highest value of the range, or `NULL` for no upper limit.
#' @return A range, to be given as the value of a filter to [audioblast()].
#' @export
#' @examples
#' ab_range(10, 20)
#' ab_range(min=10)
#' ab_range(max=20)
#' \dontrun{
#' audioblast("data", "recordings", duration=ab_range(10, 20))
#' }
ab_range <- function(min=NULL, max=NULL) {
  if (is.null(min) & is.null(max)) {
    stop("A range needs at least one of min and max.")
  }
  if (length(min) > 1 | length(max) > 1) {
    stop("min and max must each be a single value.")
  }
  if (!is.null(min) & !is.null(max)) {
    if (min > max) {
      stop("min must not be greater than max.")
    }
    r <- paste0(.audioblast_number(min), ":", .audioblast_number(max))
  } else if (is.null(max)) {
    r <- paste0(">=", .audioblast_number(min))
  } else {
    r <- paste0("<=", .audioblast_number(max))
  }
  structure(r, class="ab_range")
}

#' @export
print.ab_range <- function(x, ...) {
  cat("<audioBlast range>", unclass(x), "\n")
  invisible(x)
}

#' audioBlast - a stitch in time saves nine
#'
#' Sanity check before sending request to audioBlast
#'
#' @param type One of data, analysis, standalone.
#' @param name Name of data or analysis source.
#' @param endpoint Optionally specify endpoint of an audioBlast module.
#' @return The module's information from audioBlast.
#' @keywords internal
#' @noRd
.audioblast_ASITSN <- function(type, name, endpoint=NULL) {
  info <- .audioblast_module_info(name)
  if (is.null(info)) {
    stop(paste(name, "module does not exist."))
  }
  if (!is.null(endpoint)) {
    if (!(endpoint %in% names(info$endpoints))) {
      stop(paste(endpoint, "is not a valid endpoint."))
    }
  }
  invisible(info)
}

#' Download audio files from audioBlast
#'
#' Downloads audio files associated with a search using the audioBlast() function.
#'
#' Where audioBlast has measured a file, the SHA-256 hash of the file
#' downloaded is checked against the hash audioBlast measured, and a file that
#' does not match is reported through `on.issue`.
#'
#' @param d Data returned from a search using audioBlast().
#' @param metadata If true saves the data in d as a csv file.
#' @param skip.existing If true will not overwrite existing files.
#' @param dir Directory to save files to.
#' @param on.issue Function to call on error or warning. By default `stop` to raise
#'   a standard R error. Setting to `warning` will instead a warning.
#' @param quiet If true will not print progress.
#' @return The paths of the files, invisibly.
#' @export
#' @importFrom utils write.csv
audioblastDownload <- function(d, metadata=TRUE, skip.existing=TRUE, dir=".", quiet=FALSE, on.issue=.audioblastIssue) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  if (metadata) {
    write.csv(d, file=file.path(dir, "metadata.csv"), row.names=FALSE)
  }
  if ("filename" %in% colnames(d)) {
    urls <- d[, "filename"]
  } else if ("recording_url" %in% colnames(d)) {
    urls <- d[, "recording_url"]
  } else {
    stop("d does not have a filename or recording_url column of files to download.")
  }
  hashes <- rep(NA_character_, length(urls))
  if ("calculated_hash" %in% colnames(d)) {
    hashes <- d[, "calculated_hash"]
  }
  names <- .audioblast_file_names(d, urls)
  keep <- !is.na(urls) & urls != "" & !duplicated(urls)
  not_url <- keep & !grepl("^https?://", urls)
  if (any(not_url)) {
    on.issue(simpleError(paste0(
      sum(not_url), " files cannot be downloaded as their source gives no URL for them, e.g. ",
      urls[not_url][1], "."
    )))
  }
  keep <- keep & !not_url
  urls <- urls[keep]
  hashes <- hashes[keep]
  paths <- file.path(dir, names[keep])

  todo <- seq_along(urls)
  if (skip.existing) {
    todo <- todo[!file.exists(paths[todo])]
  }
  if (!quiet & length(todo) < length(urls)) {
    message("Skipping ", length(urls) - length(todo), " files that have already been downloaded.")
  }
  for (i in todo) {
    tryCatch({
      if (!quiet) {
        message("Downloading ", urls[i])
      }
      .audioblast_download(urls[i], paths[i])
      if (!is.na(hashes[i]) & hashes[i] != "") {
        if (.audioblast_sha256(paths[i]) != tolower(hashes[i])) {
          warning(paste(basename(paths[i]), "does not match the hash audioBlast measured of it."))
        }
      }
    }, error=function(e) on.issue(e), warning=function(w) on.issue(w))
  }
  invisible(paths)
}

#' Download one file
#'
#' @param url URL of the file.
#' @param path Path to save the file to.
#' @keywords internal
#' @noRd
.audioblast_download <- function(url, path) {
  req <- .audioblast_user_agent(httr2::request(url))
  req <- httr2::req_retry(req, max_tries=3, retry_on_failure=TRUE)
  # Write to a temporary file, so that a failed download does not leave a
  # partial file that would later be skipped as already downloaded
  tmp <- paste0(path, ".part")
  on.exit(unlink(tmp))
  httr2::req_perform(req, path=tmp)
  if (!file.rename(tmp, path)) {
    stop("Could not save ", path, ".")
  }
  invisible(path)
}

#' Name the files that audioblastDownload() saves
#'
#' A file is named as its URL names it. A URL that does not end in a file name,
#' such as https://xeno-canto.org/1178982/download, would give every file from
#' its source the same name, so these files are named by their source and ID,
#' with the extension of their MIME type.
#'
#' @param d Data returned from a search using audioBlast().
#' @param urls URLs of the files.
#' @return File names, one for each URL.
#' @keywords internal
#' @noRd
.audioblast_file_names <- function(d, urls) {
  names <- basename(sub("[?#].*$", "", urls))
  no_ext <- !grepl("\\.[A-Za-z0-9]{1,5}$", names)
  if (any(no_ext) & all(c("source", "id") %in% colnames(d))) {
    ext <- rep("", nrow(d))
    if ("mime" %in% colnames(d)) {
      # The usual extension of common audio types comes first
      map <- c(
        mp3="audio/mpeg", wav="audio/x-wav", wav="audio/wav", m4a="audio/mp4",
        flac="audio/flac", ogg="audio/ogg", mime::mimemap
      )
      ext <- vapply(d[, "mime"], function(m) {
        e <- names(map)[map == m]
        if (length(e) == 0) "" else paste0(".", e[1])
      }, character(1))
    }
    ids <- gsub("[^A-Za-z0-9._-]", "_", paste(d[, "source"], d[, "id"], sep="_"))
    names[no_ext] <- paste0(ids[no_ext], ext[no_ext])
  }
  names
}

#' Pass a problem to the function the user chose to handle problems with
#'
#' @param cond The error or warning.
#' @param on.issue The function to handle it with.
#' @keywords internal
#' @noRd
.audioblast_issue <- function(cond, on.issue) {
  # stop() re-raises the condition itself, keeping its class; other functions,
  # such as warning() or message(), are given its message
  if (identical(on.issue, stop)) {
    stop(cond)
  }
  on.issue(conditionMessage(cond))
}

.audioblastIssue <- function(e) {
  warning(
    "There was an issue downloading the requested file via audioblast.\n",
    conditionMessage(e), "\n",
    "If this issue continues please contact the audioblast team.",
    call. = FALSE
  )
}

# Path of each record type on audioBlast, named by the module it is a record of
.audioblast_record_types <- c(
  recordings = "recording",
  annomate = "annotation",
  taxa = "taxon",
  traits = "trait",
  images = "image",
  specimens = "specimen",
  locations = "location",
  descriptions = "description",
  vernacularnames = "vernacular-name",
  onomatopoeia = "onomatopoeia",
  references = "reference",
  links = "link"
)

# Module information is cached for the session
.audioblast_cache <- new.env(parent=emptyenv())

#' Fetch the information audioBlast holds about a module
#'
#' @param name Name of the module.
#' @return The module's information, or NULL if there is no such module.
#' @keywords internal
#' @noRd
.audioblast_module_info <- function(name) {
  if (!exists(name, envir=.audioblast_cache, inherits=FALSE)) {
    res <- .audioblast_json(
      .audioblast_perform("standalone/modules/module_info", list(module=name)),
      simplifyVector=FALSE
    )
    assign(name, res$data, envir=.audioblast_cache)
  }
  get(name, envir=.audioblast_cache, inherits=FALSE)
}

#' Send a request to audioBlast
#'
#' @param path Path of the request, relative to the API.
#' @param query Named list of query parameters. An element with several values
#'   is sent as a list.
#' @param allow_404 If TRUE a 404 is returned rather than raised as an error.
#' @param slash If TRUE the path ends in a slash, as a module's path does. A
#'   record's path does not, as the ID of a record can itself contain a slash.
#' @return An httr2 response.
#' @keywords internal
#' @noRd
.audioblast_perform <- function(path, query=list(), allow_404=FALSE, slash=TRUE) {
  url <- getOption("sonicscrewdriver.audioblast_url", "https://api.audioblast.org")
  req <- httr2::request(url)
  req <- httr2::req_url_path_append(req, path)
  if (slash) {
    req <- httr2::req_url_path_append(req, "/")
  }
  for (n in names(query)) {
    v <- query[[n]]
    # A value with a comma in it can only be sent in the list form
    if (length(v) > 1 && any(grepl(",", v, fixed=TRUE))) {
      q <- stats::setNames(list(v), paste0(n, "[]"))
    } else {
      q <- stats::setNames(list(paste(v, collapse=",")), n)
    }
    req <- httr2::req_url_query(req, !!!q, .multi="explode")
  }
  req <- .audioblast_user_agent(req)
  req <- httr2::req_retry(req, max_tries=3, retry_on_failure=TRUE)
  req <- httr2::req_error(
    req,
    is_error = function(resp) {
      httr2::resp_status(resp) >= 400 & !(allow_404 & httr2::resp_status(resp) == 404)
    },
    body = .audioblast_error_body
  )
  httr2::req_perform(req)
}

.audioblast_user_agent <- function(req) {
  httr2::req_user_agent(
    req,
    paste0(
      "sonicscrewdriver/", utils::packageVersion("sonicscrewdriver"),
      " (https://github.com/edwbaker/SonicScrewdriveR)"
    )
  )
}

# audioBlast explains a refused request in the body of the response
.audioblast_error_body <- function(resp) {
  if (!httr2::resp_has_body(resp)) {
    return(NULL)
  }
  body <- tryCatch(httr2::resp_body_string(resp), error=function(e) "")
  if (grepl("json", httr2::resp_content_type(resp), fixed=TRUE)) {
    notes <- tryCatch(jsonlite::fromJSON(body)$notes, error=function(e) NULL)
    body <- paste(unlist(notes[setdiff(names(notes), c("input_params", "total_execution_time", "query_execution_time"))]), collapse=" ")
  }
  body <- .audioblast_unescape(gsub("<[^>]+>", "", body))
  if (trimws(body) == "") {
    return(NULL)
  }
  paste("audioBlast says:", trimws(body))
}

.audioblast_unescape <- function(x) {
  entities <- c("&lt;"="<", "&gt;"=">", "&quot;"="\"", "&#039;"="'", "&amp;"="&")
  for (e in names(entities)) {
    x <- gsub(e, entities[[e]], x, fixed=TRUE)
  }
  x
}

.audioblast_json <- function(resp, simplifyVector=TRUE) {
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector=simplifyVector)
}

#' Fetch the pages of a search of an audioBlast module
#'
#' @return A data frame of the rows found, NULL if there are none, or a
#'   character vector of RDF for an RDF output.
#' @keywords internal
#' @noRd
#' @importFrom utils txtProgressBar setTxtProgressBar
.audioblast_pages <- function(path, filters, page, page_size, max_pages, quiet, output) {
  query <- c(filters, list(page=page, page_size=page_size))
  if (output %in% c("JSON-LD", "Turtle")) {
    query$output <- output
    ret <- character(0)
    last <- page + .audioblast_null(max_pages, Inf) - 1
    repeat {
      resp <- .audioblast_perform(path, query)
      ret <- c(ret, httr2::resp_body_string(resp))
      # Pages of RDF name the next page in a Link header
      if (is.null(httr2::resp_link_url(resp, "next")) | query$page >= last) {
        break
      }
      query$page <- query$page + 1
    }
    return(ret)
  }

  res <- .audioblast_json(.audioblast_perform(path, query))
  #A query matching nothing has come back both ways: without a data field at
  #all, and with an empty one and a last page of zero. Either way there is
  #nothing to return and it is not an error. NROW() rather than length(),
  #which counts the columns of a data frame and so never reaches zero.
  if (!"data" %in% names(res) || NROW(res$data) == 0) {
    return(NULL)
  }
  rets <- list(res$data)
  last <- min(res$last_page, page + .audioblast_null(max_pages, Inf) - 1)
  show_progress <- !quiet & last > page
  if (show_progress) {
    pb <- txtProgressBar(min = page, max = last, initial = page, style = 3)
    on.exit(close(pb))
  }
  while (query$page < last) {
    query$page <- query$page + 1
    res <- .audioblast_json(.audioblast_perform(path, query))
    rets[[length(rets) + 1]] <- res$data
    if (show_progress) {
      setTxtProgressBar(pb, query$page)
    }
  }
  do.call(rbind, rets)
}

#' Prepare filters for sending to audioBlast
#'
#' @param args Named list of filters.
#' @return Named list of filters as character vectors.
#' @keywords internal
#' @noRd
.audioblast_filters <- function(args) {
  if (length(args) == 0) {
    return(list())
  }
  nams <- names(args)
  if (is.null(nams) || any(nams == "")) {
    stop("Every filter must be named, e.g. taxon = \"Gryllotalpa vineae\".")
  }
  if (any(duplicated(nams))) {
    stop("Filter ", nams[duplicated(nams)][1], " was given more than once. Give all of its values at once, e.g. id = c(1, 2).")
  }
  args <- args[!vapply(args, function(v) is.null(v) || all(is.na(v)), logical(1))]
  lapply(args, function(v) {
    if (inherits(v, "ab_range")) {
      return(unclass(v))
    }
    if (is.numeric(v)) {
      return(.audioblast_number(v))
    }
    as.character(v)
  })
}

#' Split filters with more values than audioBlast takes into batches
#'
#' @param filters Named list of filters.
#' @param size Most values audioBlast takes for a filter.
#' @return A list of named lists of filters.
#' @keywords internal
#' @noRd
.audioblast_batches <- function(filters, size=100) {
  long <- names(filters)[vapply(filters, length, integer(1)) > size]
  if (length(long) == 0) {
    return(list(filters))
  }
  if (length(long) > 1) {
    stop("Only one filter can be given more than ", size, " values, but ", paste(long, collapse=" and "), " were.")
  }
  values <- unique(filters[[long]])
  chunks <- split(values, ceiling(seq_along(values) / size))
  lapply(chunks, function(chunk) {
    f <- filters
    f[[long]] <- chunk
    f
  })
}

#' Convert the columns audioBlast describes as numeric to numbers
#'
#' @param d Data frame returned by audioBlast.
#' @param info The module's information.
#' @return The data frame with numeric columns converted.
#' @keywords internal
#' @noRd
.audioblast_convert <- function(d, info) {
  if (is.null(info) | !is.data.frame(d)) {
    return(d)
  }
  numeric_types <- c("range", "integer", "float")
  for (col in intersect(colnames(d), names(info$params))) {
    if (!.audioblast_null(info$params[[col]]$type, "") %in% numeric_types) {
      next
    }
    v <- d[[col]]
    if (!is.character(v)) {
      next
    }
    v[!is.na(v) & trimws(v) == ""] <- NA
    n <- suppressWarnings(as.numeric(v))
    # Leave a column alone if any of its values are not numbers
    if (all(is.na(n) == is.na(v))) {
      d[[col]] <- n
    }
  }
  d
}

#' Convert annomate results to Annotation objects
#' @keywords internal
#' @noRd
.audioblast_annotations <- function(ret) {
  lapply(seq_len(nrow(ret)), function(i) {
    annotation(
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
      low = .audioblast_frequency(ret[i, "freq_low"], 0),
      high = .audioblast_frequency(ret[i, "freq_high"], Inf),
      type = ret[i, "type"],
      value = ret[i, "taxon"]
    )
  })
}

#' A frequency limit of an annotation from audioBlast
#'
#' An annotation that gives no limit has none, which an Annotation object holds
#' as the limit of the recording: 0 for the lowest frequency and `Inf` for the
#' highest.
#' @param x The limit audioBlast gives, which may be NA
#' @param none The limit to use where audioBlast gives none
#' @keywords internal
#' @noRd
.audioblast_frequency <- function(x, none) {
  x <- as.numeric(x)
  if (is.na(x)) {
    return(none)
  }
  return(x)
}

.audioblast_number <- function(x) {
  format(x, scientific=FALSE, trim=TRUE, digits=15)
}

.audioblast_null <- function(x, default) {
  if (is.null(x)) default else x
}

.audioblast_sha256 <- function(path) {
  return(digest::digest(file=path, algo="sha256"))
}
