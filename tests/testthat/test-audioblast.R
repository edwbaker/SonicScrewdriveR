test_that("ASITSN rejects unknown input", {
  local_audioblast()
  expect_error(
    .audioblast_ASITSN("standalone", "chipmunk"),
    "chipmunk module does not exist."
  )
  expect_error(
    .audioblast_ASITSN("data", "recordings", "ground hog"),
    "ground hog is not a valid endpoint."
  )
})

test_that("audioblast rejects bad arguments", {
  local_audioblast()
  expect_error(
    audioblast("data", "recordings", source="bio.acousti.ca", output="koala"),
    "koala is not a valid output type."
  )
  expect_error(
    audioblast("data", "recordings", max_pages=1, output="Annotation"),
    "Query does not gives results that can be turned into Annotation objects."
  )
  expect_error(audioblast("data", "recordings", page_size=0), "page_size")
  expect_error(audioblast("data", "recordings", page_size=c(10, 20)), "page_size")
  expect_error(audioblast("data", "recordings", max_pages=0), "max_pages")
  expect_silent(
    audioblast("data", "annomate", max_pages=1, on.issue=invisible, output="Annotation")
  )
})

test_that("audioblast works with real data", {
  local_audioblast()
  data <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca")
  expect_equal(nrow(data), 50)

  data2 <- audioblast("data", "recordings", max_pages = 2, source="bio.acousti.ca", quiet=TRUE)
  expect_equal(nrow(data2), 100)

  data2 <- audioblast("data", "recordings", max_pages = 1, page = 2, source="bio.acousti.ca")
  expect_false(any(data2$id %in% data$id))

  data <- audioblast("standalone", "analysis", "fetch_analysis_counts", source="unp", id="nhm-unp-1-1588606870", )
  # audioBLAST adds fields to its responses as it grows, and has since added
  # "unavailable" to this one, so the fields asked for are checked to be there
  # rather than to be all that is there.
  expect_true(all(c("counts", "total") %in% names(data)))

  expect_true(is.null(audioblast("data", "recordings", source="unp", deployment="horsefly", )))
})

test_that("audioblastRecord works with real data", {
  local_audioblast()
  r <- audioblastRecord("recording", "bio.acousti.ca", 10754)
  expect_equal(nrow(r), 1)
  expect_equal(r$id, "10754")
  expect_type(r$duration, "double")

  expect_null(audioblastRecord("recording", "bio.acousti.ca", 99999999))
  expect_match(audioblastRecord("taxon", "bio.acousti.ca", 247, output="Turtle"), "@prefix", fixed=TRUE)
  expect_error(audioblastRecord("koala", "bio.acousti.ca", 1))
})

test_that("audioblastFilters works with real data", {
  local_audioblast()
  f <- audioblastFilters("recordings")
  expect_true(all(c("source", "id", "taxon", "duration") %in% f$name))
  expect_false("output" %in% f$name)
  expect_true(f$multiple[f$name == "id"])
  expect_equal(f$type[f$name == "duration"], "range")
  expect_error(audioblastFilters("chipmunk"), "chipmunk module does not exist.")
})

test_that("Annotation output is as expected", {
  local_audioblast()
  # Get annotations from audioblast as data.frame
  data <- audioblast("data", "annomate", max_pages = 1, source="bio.acousti.ca")
  expect_true(is.data.frame(data))

  # Get annotations from audioblast as Annotation objects
  anns <- audioblast("data", "annomate", max_pages = 1, source="bio.acousti.ca", output="Annotation")
  expect_true(all(sapply(anns, inherits, "Annotation")))

  d <- data[2,]
  a <- anns[[2]]
  expect_equal(as.numeric(d$time_start), a@start)
  expect_equal(as.numeric(d$time_end), a@end)
  expect_equal(d$source, a@metadata$source)
  # These annotations give no frequency limits, so they span every frequency.
  expect_identical(c(a@low, a@high), c(0, Inf))
})

test_that("the frequency limits of an annotation from audioBlast are kept", {
  # They were left out, so every annotation spanned every frequency.
  ret <- data.frame(
    source="s", source_id="1", annotator="a", annotation_id="1",
    annotation_date="", annotation_info_url="", lat=NA, lon=NA, contact="",
    recording_url="r.wav", time_start=1, time_end=2,
    freq_low=c(1000, NA), freq_high=c("8000.0", NA), type="Call", taxon="t",
    stringsAsFactors=FALSE
  )
  anns <- .audioblast_annotations(ret)
  expect_identical(c(anns[[1]]@low, anns[[1]]@high), c(1000, 8000))
  expect_identical(c(anns[[2]]@low, anns[[2]]@high), c(0, Inf))
})

test_that("a record of audioBlast whose limits run backwards does not lose the others", {
  # An Annotation that ends before it starts is refused, which would have
  # refused the whole of a fetch for one record.
  ret <- data.frame(
    source="s", source_id="1", annotator="a", annotation_id=c("ok", "late", "high"),
    annotation_date="", annotation_info_url="", lat=NA, lon=NA, contact="",
    recording_url="r.wav", time_start=c(1, 5, 1), time_end=c(2, 3, 2),
    freq_low=c(100, 100, 9000), freq_high=c(200, 200, 8000), type="Call", taxon="t",
    stringsAsFactors=FALSE
  )
  expect_message(anns <- .audioblast_annotations(ret),
                 "2 annotations ending before they start.*\\(late, high\\)")
  expect_length(anns, 3)
  expect_identical(c(anns[[1]]@start, anns[[1]]@end, anns[[1]]@low, anns[[1]]@high),
                   c(1, 2, 100, 200))
  expect_identical(c(anns[[2]]@start, anns[[2]]@end), c(NA_real_, NA_real_))
  expect_identical(c(anns[[2]]@low, anns[[2]]@high), c(100, 200))
  expect_identical(c(anns[[3]]@start, anns[[3]]@end), c(1, 2))
  expect_identical(c(anns[[3]]@low, anns[[3]]@high), c(NA_real_, NA_real_))

  expect_message(.audioblast_annotations(ret[2, ]), "1 annotation ending before it starts.*\\(late\\)")
  expect_silent(.audioblast_annotations(ret[1, ]))
})

test_that("annotations asked for as they were spelt before still come, with a warning", {
  local_audioblast()
  expect_warning(
    old <- audioblast("data", "annomate", max_pages=1, source="bio.acousti.ca", output="Annotations"),
    'output="Annotations" is deprecated'
  )
  new <- audioblast("data", "annomate", max_pages=1, source="bio.acousti.ca", output="Annotation")
  expect_identical(old, new)
})

# ------------------------------------------------------------- the pages ---

#The pages below never leave the machine: httr2 is told what to answer, so what
#is checked is which pages are asked for and how they are joined together.
pagedApi <- function(last_page=3, env=parent.frame()) {
  asked <- new.env(parent=emptyenv())
  asked$urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    asked$urls <- c(asked$urls, req$url)
    page <- as.integer(regmatches(req$url, regexpr("(?<=[?&]page=)[0-9]+", req$url, perl=TRUE)))
    httr2::response_json(body=list(
      data = list(list(page=page, taxon=paste("taxon", page))),
      last_page = last_page,
      params = list()
    ))
  }, env=env)
  return(asked)
}

test_that("every page of results is fetched and joined to the first", {
  api <- pagedApi()
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_equal(res$page, 1:3)
  expect_equal(nrow(res), 3)
  expect_length(api$urls, 3)
})

test_that("no more than the maximum number of pages asked for is fetched", {
  api <- pagedApi(last_page=10)
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, max_pages=2)
  expect_equal(res$page, 1:2)
  expect_length(api$urls, 2)
})

#As pagedApi(), but the page count can be left out of the body altogether or
#sent as a string. jsonlite gives NULL for an absent key and for a JSON null
#alike, so leaving it out covers both. The requests are capped because the loop
#this guards had no terminating condition of its own: without the cap a
#regression here would keep asking for pages until the test timed out.
oddPageCountApi <- function(last_page=NULL, cap=15, env=parent.frame()) {
  asked <- new.env(parent=emptyenv())
  asked$urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    asked$urls <- c(asked$urls, req$url)
    if (length(asked$urls) > cap) {
      stop("asked for more than ", cap, " pages")
    }
    page <- as.integer(regmatches(req$url, regexpr("(?<=[?&]page=)[0-9]+", req$url, perl=TRUE)))
    body <- list(data = list(list(page=page, taxon=paste("taxon", page))))
    body$last_page <- last_page
    body$params <- list()
    httr2::response_json(body=body)
  }, env=env)
  return(asked)
}

test_that("a response that gives no page count is read as the only page", {
  #min(NULL, Inf) is Inf, so the loop had no terminating condition of its own:
  #it ran until the API refused the page past the last, and that refusal is
  #raised as an error, so every row already fetched was thrown away.
  api <- oddPageCountApi()
  expect_message(
    res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE),
    "did not say how many pages"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$page, 1)
  expect_length(api$urls, 1)
})

test_that("a page count that is not a number is read as the only page", {
  api <- oddPageCountApi(last_page="several")
  expect_message(
    res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE),
    "did not say how many pages"
  )
  expect_equal(nrow(res), 1)
  expect_length(api$urls, 1)
})

test_that("max_pages still bounds a search whose response gives no page count", {
  api <- oddPageCountApi()
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, max_pages=3)
  expect_equal(res$page, 1:3)
  expect_length(api$urls, 3)
})

test_that("a page count sent as a string is read as a number, not as text", {
  #min() coerced to character, and "2" < "10" is FALSE, so a ten page search
  #stopped after two pages and reported success.
  api <- oddPageCountApi(last_page="10")
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_equal(res$page, 1:10)
  expect_length(api$urls, 10)
})

# An endpoint of a module answers in one response: asked for
# data/annomate/taxa/ the API gives neither a data field nor a last page, where
# a search of a module gives both. So an endpoint is asked once, and the test
# that used to check the endpoint survived every page no longer has pages to
# check. What it was guarding against, a path that changes between requests, is
# covered for a search by the tests above.
test_that("an endpoint is asked for once, in the path, without paging", {
  api <- pagedApi()
  audioblast("data", "annomate", endpoint="taxa", check=FALSE, quiet=TRUE)
  expect_length(api$urls, 1)
  expect_match(api$urls[1], "/data/annomate/taxa/", fixed=TRUE)
})

test_that("the fields filtered on are kept on every page", {
  api <- pagedApi()
  audioblast("data", "recordings", check=FALSE, quiet=TRUE, taxon="Gryllotalpa")
  expect_length(api$urls, 3)
  expect_true(all(grepl("taxon=Gryllotalpa", api$urls, fixed=TRUE)))
})

test_that("the page size asked for is sent, and defaults to fifty", {
  api <- pagedApi(last_page=1)
  audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_true(all(grepl("page_size=50", api$urls, fixed=TRUE)))

  api <- pagedApi(last_page=1)
  audioblast("data", "recordings", check=FALSE, quiet=TRUE, page_size=10)
  expect_true(all(grepl("page_size=10", api$urls, fixed=TRUE)))
})

test_that("progress through the pages is reported unless silence is asked for", {
  pagedApi()
  expect_output(audioblast("data", "recordings", check=FALSE, quiet=FALSE))
  expect_silent(audioblast("data", "recordings", check=FALSE, quiet=TRUE))
})

test_that("a single page is fetched silently without a progress bar", {
  pagedApi(last_page=1)
  expect_silent(audioblast("data", "recordings", check=FALSE, max_pages=1))
})

test_that("a query matching nothing gives nothing rather than an error", {
  # audioBlast has answered an empty search both ways: with no data field at
  # all, and with an empty one and a last page of zero.
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body=list(params=list(), last_page=0))
  })
  expect_null(audioblast("data", "recordings", check=FALSE, quiet=TRUE))

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body=list(data=list(), last_page=0, params=list()))
  })
  expect_null(audioblast("data", "recordings", check=FALSE, quiet=TRUE))
})

#The shapes below are kept as the JSON audioBlast sends rather than built from
#R lists as the mocks above are, because a list cannot hold a null: a field a
#response carries with nothing in it is one of the shapes under test, and
#jsonlite leaves a NULL element out of what it writes altogether. They are
#responses constructed to carry one shape each, not recordings of the API, so
#they are kept apart from the saved responses alongside them.
shapeApi <- function(..., env=parent.frame()) {
  bodies <- vapply(c(...), function(n) {
    path <- test_path("fixtures", "audioblast", "shapes", paste0(n, ".json"))
    paste(readLines(path, warn=FALSE), collapse="\n")
  }, character(1))
  asked <- new.env(parent=emptyenv())
  asked$urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    asked$urls <- c(asked$urls, req$url)
    httr2::response(
      status_code=200,
      headers=list("Content-Type"="application/json"),
      body=charToRaw(bodies[[min(length(asked$urls), length(bodies))]])
    )
  }, env=env)
  return(asked)
}

test_that("pages that do not carry the same fields are joined on all of them", {
  #rbind() needs the same columns in each of its arguments, so a search whose
  #pages differed in them failed with "numbers of columns of arguments do not
  #match" once both pages had been fetched, losing the records of both.
  api <- shapeApi("pages-differ-1", "pages-differ-2")
  expect_message(
    res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE),
    "audioBlast gave 1 field for only some of the records.*\\(deployment\\)"
  )
  expect_length(api$urls, 2)
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("source", "id", "taxon", "deployment"))
  expect_equal(res$id, c("1", "2"))
  #The field is NA for the page that came without it, and keeps the type of
  #the page that carried it rather than becoming a logical column
  expect_equal(res$deployment, c(NA, "hedge"))
  expect_type(res$deployment, "character")
})

test_that("a field the first page carried and a later one did not is kept", {
  api <- shapeApi("pages-differ-1", "page-fewer-fields")
  expect_message(
    res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE),
    "\\(taxon\\)"
  )
  expect_equal(colnames(res), c("source", "id", "taxon"))
  expect_equal(res$taxon, c("Gryllotalpa vineae", NA))
})

test_that("the batches of a long filter are joined on all their fields too", {
  #The batches of a filter given more than a hundred values are by
  #construction different records, and so the likelier of the two joins to
  #meet records from sources that do not hold the same fields.
  api <- shapeApi("batch-differ-1", "batch-differ-2")
  expect_message(
    res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, id=as.character(1:150)),
    "\\(deployment\\)"
  )
  expect_length(api$urls, 2)
  expect_equal(res$id, c("1", "101"))
  expect_equal(res$deployment, c(NA, "hedge"))
})

test_that("pages that do carry the same fields are joined without a word", {
  api <- shapeApi("pages-differ-1", "pages-differ-1")
  expect_silent(res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE))
  expect_length(api$urls, 2)
  expect_equal(nrow(res), 2)
})

test_that("a page audioBlast sends as a single record is read as one row", {
  #The record itself rather than an array holding it, which jsonlite reads as
  #a list of its fields: rbind() of a list of fields gives a matrix of them,
  #so the search returned a matrix where a data frame was documented.
  shapeApi("page-one-record-object")
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$taxon, "Gryllotalpa vineae")
})

test_that("a field a page carries with nothing in it is NA rather than absent", {
  shapeApi("page-null-field")
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_true("deployment" %in% colnames(res))
  expect_true(is.na(res$deployment))
})

test_that("a page carrying no records is passed over rather than joined", {
  #An empty page joined to a page of records failed in rbind() as a page of
  #different fields did.
  api <- shapeApi("pages-differ-1", "page-no-records")
  res <- audioblast("data", "recordings", check=FALSE, quiet=TRUE)
  expect_length(api$urls, 2)
  expect_equal(nrow(res), 1)
  expect_equal(res$id, "1")
})

test_that(".audioblast_bind() joins what it is given, or gives nothing", {
  expect_null(.audioblast_bind(list()))
  expect_null(.audioblast_bind(list(NULL, list())))
  d <- data.frame(id="1", stringsAsFactors=FALSE)
  expect_identical(.audioblast_bind(list(d, NULL)), d)
})

test_that(".audioblast_bind() keeps a field that holds a record of its own", {
  #jsonlite reads such a field as a data frame within the data frame, which a
  #logical NA in the rows that came without it would mangle.
  one <- data.frame(id="1", stringsAsFactors=FALSE)
  two <- data.frame(id="2", stringsAsFactors=FALSE)
  two$position <- data.frame(lat=51.5, lon=-0.1)
  expect_message(joined <- .audioblast_bind(list(one, two)), "\\(position\\)")
  expect_s3_class(joined$position, "data.frame")
  expect_equal(joined$position$lat, c(NA, 51.5))
})

test_that(".audioblast_rows() reads a field of several values as it came", {
  rows <- .audioblast_rows(list(id="1", counts=c(1, 2, 3)))
  expect_equal(nrow(rows), 1)
  expect_equal(rows$counts[[1]], c(1, 2, 3))
})

# ---------------------------------------------------------- the requests ---

test_that("audioblast() requests are built correctly", {
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    if (grepl("module_info", req$url, fixed=TRUE)) {
      return(httr2::response_json(body=list(data=list(
        mname="mockrecordings",
        params=list(duration=list(type="range"))
      ))))
    }
    page <- as.integer(sub(".*[?&]page=([0-9]+).*", "\\1", req$url))
    httr2::response_json(body=list(
      data=list(list(id=as.character(page), duration=as.character(page * 10))),
      last_page=3,
      params=list()
    ))
  })
  d <- audioblast("data", "mockrecordings", id=c("a", "b,c"), taxon="A & B", quiet=TRUE)
  expect_equal(d$id, c("1", "2", "3"))
  expect_equal(d$duration, c(10, 20, 30))
  q <- seen[grepl("data/mockrecordings/", seen, fixed=TRUE)][1]
  expect_match(q, "id%5B%5D=a&id%5B%5D=b%2Cc", fixed=TRUE)
  expect_match(q, "taxon=A%20%26%20B", fixed=TRUE)
  expect_match(q, "page_size=50", fixed=TRUE)

  seen <- character(0)
  d <- audioblast("data", "mockrecordings", max_pages=2, page_size=10, quiet=TRUE, id=c(1, 2))
  expect_equal(nrow(d), 2)
  expect_match(seen[grepl("data/mockrecordings/", seen, fixed=TRUE)][1], "id=1%2C2", fixed=TRUE)
})

test_that("audioblast() reports what audioBlast refuses", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code=400,
      headers=list("Content-Type"="text/html; charset=UTF-8"),
      body=charToRaw("Parameter `bogus` is not recognised. It also takes: format, output, page, page_size.")
    )
  })
  expect_error(
    audioblast("data", "recordings", check=FALSE, bogus=1),
    "Parameter `bogus` is not recognised"
  )
  expect_warning(
    audioblast("data", "recordings", check=FALSE, bogus=1, on.issue=warning),
    "Parameter `bogus` is not recognised"
  )
})

test_that("ab_range() makes ranges", {
  expect_equal(unclass(ab_range(10, 20)), "10:20")
  expect_equal(unclass(ab_range(min=10)), ">=10")
  expect_equal(unclass(ab_range(max=20)), "<=20")
  expect_equal(unclass(ab_range(-0.5, 1e6)), "-0.5:1000000")
  expect_s3_class(ab_range(10, 20), "ab_range")
  expect_output(print(ab_range(10, 20)), "10:20")
  expect_error(ab_range(), "at least one of min and max")
  expect_error(ab_range(20, 10), "min must not be greater than max")
  expect_error(ab_range(c(1, 2), 10), "single value")
})

test_that(".audioblast_filters() prepares filters", {
  expect_equal(.audioblast_filters(list()), list())
  f <- .audioblast_filters(list(id=c(1, 20000000), taxon="Gryllotalpa vineae", duration=ab_range(10, 20), lat=NULL))
  expect_equal(f$id, c("1", "20000000"))
  expect_equal(f$taxon, "Gryllotalpa vineae")
  expect_equal(f$duration, "10:20")
  expect_false("lat" %in% names(f))
  expect_error(.audioblast_filters(list("x")), "must be named")
  expect_error(.audioblast_filters(list(id=1, id=2)), "more than once")
})

test_that("a filter given NA is refused by name rather than dropped", {
  #A filter that came out NA - from a column of a data frame, a lookup that
  #failed, a parseFilename() result - was dropped and the query ran without
  #it, so a search for one taxon silently became a search for the whole
  #module: the most expensive direction for a query to fail in.
  expect_error(
    .audioblast_filters(list(taxon=NA, source="x")),
    "Filter taxon was given NA"
  )
  expect_error(.audioblast_filters(list(id=c(1, NA))), "Filter id was given NA")
  expect_error(
    .audioblast_filters(list(taxon=NA, source=NA)),
    "Filters taxon and source were given NA"
  )
  #The filters are read before the query is sent, so this is an error whatever
  #on.issue says, as the other arguments of audioblast() are. A mock stands in
  #for the API to be certain nothing is asked of it, because the behaviour
  #under test is a request that is not sent: dropping the filter asks
  #audioBlast for every record the module has, page after page.
  asked <- 0
  httr2::local_mocked_responses(function(req) {
    asked <<- asked + 1
    httr2::response_json(body=list(data=list(list(id="1")), last_page=1, params=list()))
  })
  expect_error(
    audioblast("data", "recordings", check=FALSE, taxon=NA),
    "Filter taxon was given NA"
  )
  expect_equal(asked, 0)
})

test_that("a filter given no values at all is refused by name too", {
  #all(is.na(character(0))) is TRUE, so a filter holding nothing went the same
  #way as one given NA.
  expect_error(
    .audioblast_filters(list(id=character(0))),
    "Filter id was given no values to match"
  )
  expect_error(
    .audioblast_filters(list(id=integer(0), taxon=character(0))),
    "Filters id and taxon were given no values to match"
  )
})

test_that("a filter given NULL is still a filter left out", {
  f <- .audioblast_filters(list(taxon="Gryllotalpa vineae", lat=NULL))
  expect_equal(names(f), "taxon")
  expect_length(.audioblast_filters(list(lat=NULL)), 0)
})

test_that("ab_range() names a bound it was given as NA", {
  #The bounds were compared before this was checked, so a range given NA
  #failed with "missing value where TRUE/FALSE needed", which names neither
  #bound; a range given only one bound was not compared at all, and
  #ab_range(NA) sent audioBlast the range ">=NA".
  expect_error(
    ab_range(NA, 20),
    "A range needs a value at each end it is given, and min is NA"
  )
  expect_error(ab_range(10, NA), "max is NA")
  expect_error(ab_range(NA, NA), "min and max are NA")
  expect_error(ab_range(min=NA), "min is NA")
  expect_error(ab_range(max=NA_real_), "max is NA")
})

test_that("a file whose MIME type audioBlast does not hold keeps its extension off", {
  #names(map)[map == NA] is a vector of NAs rather than empty, so length(e) was
  #not zero and the file was named "xc_1.NA".
  d <- data.frame(
    source="xc", id="1", filename="https://xeno-canto.org/1/download",
    mime=NA_character_, stringsAsFactors=FALSE
  )
  expect_equal(.audioblast_file_names(d, d$filename), "xc_1")
  #A type that is not one of the extensions known gave no extension already
  d$mime <- "audio/unheard-of"
  expect_equal(.audioblast_file_names(d, d$filename), "xc_1")
  d$mime <- "audio/mpeg"
  expect_equal(.audioblast_file_names(d, d$filename), "xc_1.mp3")
})

test_that("a filter value holding a comma is sent as one value, not as two", {
  #A comma in the joined form is the separator between two values, so a name
  #holding one was sent as two names. The list form was used for a filter given
  #several values, but not for the same comma in a single value.
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    httr2::response_json(body=list(data=list(list(id="1")), last_page=1, params=list()))
  })
  audioblast("data", "recordings", check=FALSE, quiet=TRUE,
             taxon="Oecanthus pellucens, dubius")
  expect_match(seen[1], "taxon%5B%5D=Oecanthus%20pellucens%2C%20dubius", fixed=TRUE)
})

test_that("a taxon name with spaces and brackets is sent as it was given", {
  #audioBlast writes a subgenus as "Gryllus Gryllus campestris", and a name
  #with brackets in it is routine, so both are escaped for the query rather
  #than sent as they are.
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    httr2::response_json(body=list(data=list(list(id="1")), last_page=1, params=list()))
  })
  audioblast("data", "recordings", check=FALSE, quiet=TRUE,
             taxon="Gryllus (Gryllus) campestris")
  expect_match(seen[1], "taxon=Gryllus%20%28Gryllus%29%20campestris", fixed=TRUE)

  seen <- character(0)
  audioblast("data", "recordings", check=FALSE, quiet=TRUE,
             taxon=c("Gryllus Gryllus campestris", "Oecanthus pellucens"))
  expect_match(
    seen[1], "taxon=Gryllus%20Gryllus%20campestris%2COecanthus%20pellucens",
    fixed=TRUE
  )
})

test_that(".audioblast_batches() splits long filters", {
  f <- list(source="bio.acousti.ca", id=as.character(1:250))
  b <- .audioblast_batches(f)
  expect_length(b, 3)
  expect_equal(lengths(lapply(b, `[[`, "id")), c(100, 100, 50), ignore_attr=TRUE)
  expect_true(all(vapply(b, function(x) x$source == "bio.acousti.ca", logical(1))))
  expect_length(.audioblast_batches(list(id=as.character(1:100))), 1)
  expect_error(.audioblast_batches(list(id=as.character(1:101), taxon=as.character(1:101))), "Only one filter")
})

test_that(".audioblast_convert() converts numeric columns only", {
  info <- list(params=list(
    id=list(type="string"),
    duration=list(type="range"),
    value=list(type="integer")
  ))
  d <- data.frame(id=c("1", "2"), duration=c("10", ""), value=c("1", "[1,2]"))
  o <- .audioblast_convert(d, info)
  expect_type(o$id, "character")
  expect_equal(o$duration, c(10, NA))
  expect_equal(o$value, c("1", "[1,2]"))
  expect_equal(.audioblast_convert(d, NULL), d)
})

# --------------------------------------------------------- the downloads ---

test_that("audioblastDownload works as expected", {
  local_audioblast()
  dir <- file.path(tempdir(), "ab_dl_test")
  unlink(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  recs <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca", id="11096", )

  paths <- audioblastDownload(recs, dir=dir, metadata = FALSE, quiet=TRUE, )
  expect_true(dir.exists(dir))
  expect_length(list.files(dir, pattern="\\.csv$"), 0)
  expect_true(all(file.exists(paths)))
  # A part file is renamed when the download finishes, so none is left behind.
  expect_false(any(file.exists(paste0(paths, ".part"))))

  # Files already in dir are skipped rather than fetched again
  mtime <- file.mtime(paths)
  audioblastDownload(recs, dir=dir, metadata = TRUE, quiet=TRUE, )
  expect_equal(file.mtime(paths), mtime)
  expect_length(list.files(dir, pattern="\\.csv$"), 1)
})

test_that("audioblastDownload names and checks files", {
  dir <- file.path(tempdir(), "ab_hash_test")
  unlink(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  local_mocked_bindings(.audioblast_download = function(url, path) {
    writeBin(charToRaw("not a wave"), path)
    invisible(path)
  })

  # URLs that do not end in a file name are named by source and ID
  d <- data.frame(
    source="xeno-canto", id=c("1", "2"), mime="audio/mpeg",
    filename=c("https://xeno-canto.org/1/download", "https://xeno-canto.org/2/download")
  )
  paths <- audioblastDownload(d, dir=dir, metadata=FALSE, quiet=TRUE)
  expect_equal(basename(paths), c("xeno-canto_1.mp3", "xeno-canto_2.mp3"))
  expect_true(all(file.exists(paths)))

  # A file that does not match the hash audioBlast measured is reported
  d <- data.frame(filename="https://example.org/a.wav", calculated_hash="00")
  expect_warning(
    audioblastDownload(d, dir=dir, metadata=FALSE, quiet=TRUE),
    "does not match the hash"
  )
  d$calculated_hash <- .audioblast_sha256(file.path(dir, "a.wav"))
  unlink(file.path(dir, "a.wav"))
  expect_silent(audioblastDownload(d, dir=dir, metadata=FALSE, quiet=TRUE))

  # A file with no URL cannot be downloaded
  d <- data.frame(filename="audiomoth/site1/20220716_133000.WAV")
  expect_warning(audioblastDownload(d, dir=dir, metadata=FALSE, quiet=TRUE), "no URL")

  expect_error(audioblastDownload(data.frame(x=1), dir=dir, metadata=FALSE), "filename or recording_url")
})

test_that("a download that fails is reported as a warning naming audioblast", {
  expect_warning(
    sonicscrewdriver:::.audioblastIssue(simpleError("server said no")),
    "issue downloading the requested file via audioblast"
  )
  expect_warning(
    sonicscrewdriver:::.audioblastIssue(simpleError("server said no")),
    "server said no"
  )
  expect_warning(
    sonicscrewdriver:::.audioblastIssue(simpleError("server said no")),
    "contact the audioblast team"
  )
})

test_that("a failed download is a warning by default, so the rest are still tried", {
  dir <- file.path(tempdir(), paste0("ab-", basename(tempfile(""))))
  withr::defer(unlink(dir, recursive=TRUE))
  d <- data.frame(filename=c(
    "https://invalid.invalid/first.wav",
    "https://invalid.invalid/second.wav"
  ))
  #The download is stood in for so that nothing is fetched.
  attempted <- character(0)
  local_mocked_bindings(
    .audioblast_download = function(url, path) {
      attempted <<- c(attempted, url)
      stop("could not resolve host")
    }
  )
  #One warning for each file that failed, and the second file was still tried.
  expect_warning(
    expect_warning(audioblastDownload(d, dir=dir, quiet=TRUE), "issue downloading"),
    "issue downloading"
  )
  expect_equal(attempted, d$filename)
  expect_true(file.exists(file.path(dir, "metadata.csv")))
})

test_that("A failed download leaves no file behind", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code=403))
  path <- file.path(tempdir(), "ab_failed.wav")
  withr::defer(unlink(c(path, paste0(path, ".part"))))
  expect_error(.audioblast_download("https://example.org/a.wav", path), "403")
  expect_false(file.exists(path))
  expect_false(file.exists(paste0(path, ".part")))
})

# ------------------------------------------------------- the linked data ---

test_that("a search can give linked data, a page at a time", {
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    page <- as.integer(sub(".*[?&]page=([0-9]+).*", "\\1", req$url))
    headers <- list("Content-Type"="text/turtle")
    # A page of RDF names the next one in a Link header, and the last names none
    if (page < 2) {
      headers[["Link"]] <- paste0(
        "<", sub("page=1", "page=2", req$url, fixed=TRUE), ">; rel=\"next\""
      )
    }
    httr2::response(
      status_code=200, headers=headers,
      body=charToRaw(paste0("@prefix p", page, ": <http://example.org/> ."))
    )
  })
  rdf <- audioblast("data", "recordings", check=FALSE, quiet=TRUE, output="Turtle")

  expect_length(rdf, 2)
  expect_match(rdf[1], "@prefix p1", fixed=TRUE)
  expect_match(rdf[2], "@prefix p2", fixed=TRUE)
  expect_true(all(grepl("output=Turtle", seen, fixed=TRUE)))
})

test_that("only as many pages of linked data as were asked for are fetched", {
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    httr2::response(
      status_code=200,
      headers=list(
        "Content-Type"="text/turtle",
        "Link"="<https://api.audioblast.org/x?page=9>; rel=\"next\""
      ),
      body=charToRaw("@prefix p: <http://example.org/> .")
    )
  })
  rdf <- audioblast("data", "recordings", check=FALSE, quiet=TRUE,
                    output="JSON-LD", max_pages=2)

  expect_length(rdf, 2)
  expect_length(seen, 2)
})

test_that("linked data is only offered for a search of a module", {
  expect_error(
    audioblast("standalone", "modules", check=FALSE, output="JSON-LD"),
    "only available from searches"
  )
  expect_error(
    audioblast("data", "annomate", endpoint="taxa", check=FALSE, output="Turtle"),
    "only available from searches"
  )
})

test_that("a standalone module that answers with nothing gives nothing", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body=list(data=list(), params=list()))
  })
  expect_null(
    audioblast("standalone", "analysis", "fetch_analysis_counts", check=FALSE)
  )
})

test_that("a record that is not there gives nothing", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code=404))
  expect_null(audioblastRecord("recording", "bio.acousti.ca", 1))
})

test_that("a filter of more than a hundred values is sent in batches", {
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    httr2::response_json(body=list(
      data=list(list(id=as.character(length(seen)))), last_page=1, params=list()
    ))
  })
  d <- audioblast("data", "recordings", check=FALSE, quiet=TRUE,
                  id=as.character(1:150))

  # A hundred to a batch, so two requests, and the rows of both come back
  expect_length(seen, 2)
  expect_equal(nrow(d), 2)
})

# -------------------------------------------- what audioBlast says of it ---

test_that("a refusal explained in JSON is read from the notes it holds", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code=400,
      headers=list("Content-Type"="application/json"),
      body=charToRaw(jsonlite::toJSON(list(notes=list(
        error="Parameter bogus is not recognised.",
        input_params=list(bogus="1"),
        total_execution_time="0.1"
      )), auto_unbox=TRUE))
    )
  })
  expect_error(
    audioblast("data", "recordings", check=FALSE, bogus=1),
    "Parameter bogus is not recognised"
  )
})

test_that("a refusal that explains nothing is reported as the status it was", {
  # Notes holding only what the API echoes of the request say nothing about
  # what was wrong, so there is nothing to add to the status.
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code=400,
      headers=list("Content-Type"="application/json"),
      body=charToRaw(jsonlite::toJSON(list(notes=list(
        input_params=list(bogus="1"), query_execution_time="0.1"
      )), auto_unbox=TRUE))
    )
  })
  expect_error(audioblast("data", "recordings", check=FALSE), "400")

  httr2::local_mocked_responses(function(req) httr2::response(status_code=500))
  expect_error(audioblast("data", "recordings", check=FALSE), "500")
})

# --------------------------------------------------- the files themselves ---

test_that("the files of a search can be named by a recording_url column", {
  dir <- file.path(tempdir(), "ab_url_col")
  unlink(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  local_mocked_bindings(.audioblast_download = function(url, path) {
    writeBin(charToRaw("RIFF"), path)
    invisible(path)
  })
  d <- data.frame(recording_url="https://example.org/one.wav")
  paths <- audioblastDownload(d, dir=dir, metadata=FALSE, quiet=TRUE)

  expect_equal(basename(paths), "one.wav")
  expect_true(file.exists(paths))
})

test_that("what is being fetched and skipped is said unless silence is asked for", {
  dir <- file.path(tempdir(), "ab_say")
  unlink(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  local_mocked_bindings(.audioblast_download = function(url, path) {
    writeBin(charToRaw("RIFF"), path)
    invisible(path)
  })
  d <- data.frame(filename="https://example.org/two.wav")

  expect_message(
    audioblastDownload(d, dir=dir, metadata=FALSE, quiet=FALSE),
    "Downloading https://example.org/two.wav"
  )
  # The second time the file is there already, so it is skipped and said so
  expect_message(
    audioblastDownload(d, dir=dir, metadata=FALSE, quiet=FALSE),
    "Skipping 1 file"
  )
})

test_that("a file is moved into place once it is complete", {
  # A mocked response writes no file, so the part file is left where the
  # download would have left it: what this tests is that it is moved into place
  # and reported, rather than how it was fetched.
  path <- file.path(tempdir(), "ab_move.wav")
  part <- paste0(path, ".part")
  unlink(c(path, part))
  withr::defer(unlink(c(path, part)))
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code=200, body=charToRaw("RIFF"))
  })
  writeBin(charToRaw("RIFF"), part)

  expect_equal(.audioblast_download("https://example.org/a.wav", path), path)
  expect_true(file.exists(path))
  expect_false(file.exists(part))
})

test_that("a file that cannot be moved into place says so", {
  # A directory already in the way of the name the file is to be given.
  dir <- file.path(tempdir(), "ab_in_the_way.wav")
  dir.create(dir, showWarnings=FALSE)
  withr::defer(unlink(dir, recursive=TRUE))
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code=200, body=charToRaw("RIFF"))
  })
  writeBin(charToRaw("RIFF"), paste0(dir, ".part"))

  # R gives a warning of its own that the rename failed, which is the condition
  # under test rather than something the suite should carry.
  expect_error(
    suppressWarnings(.audioblast_download("https://example.org/a.wav", dir)),
    "Could not save"
  )
})
