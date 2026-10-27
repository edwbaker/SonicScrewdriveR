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
  f <- .audioblast_filters(list(id=c(1, 20000000), taxon="Gryllotalpa vineae", duration=ab_range(10, 20), lat=NULL, lon=NA))
  expect_equal(f$id, c("1", "20000000"))
  expect_equal(f$taxon, "Gryllotalpa vineae")
  expect_equal(f$duration, "10:20")
  expect_false(any(c("lat", "lon") %in% names(f)))
  expect_error(.audioblast_filters(list("x")), "must be named")
  expect_error(.audioblast_filters(list(id=1, id=2)), "more than once")
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
      last_page=3
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

test_that("audioblast rejects bad arguments", {
  expect_error(
    audioblast("data", "recordings", source="bio.acousti.ca", output="koala"),
    "koala is not a valid output type."
  )
  expect_error(
    audioblast("data", "recordings", max_pages=1, output="Annotations"),
    "Query does not gives results that can be turned into Annotation objects."
  )
  expect_error(audioblast("data", "recordings", page_size=0), "page_size")
  expect_error(audioblast("data", "recordings", max_pages=0), "max_pages")
})

test_that("ASITSN rejects unknown input", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  expect_error(
    .audioblast_ASITSN("standalone", "chipmunk"),
    "chipmunk module does not exist."
  )
  expect_error(
    .audioblast_ASITSN("data", "recordings", "ground hog"),
    "ground hog is not a valid endpoint."
  )
})

test_that("audioblast works with real data", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  data <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca")
  expect_equal(nrow(data), 50)
  expect_type(data$duration, "double")
  expect_type(data$id, "character")

  data2 <- audioblast("data", "recordings", max_pages = 2, source="bio.acousti.ca", quiet=TRUE)
  expect_equal(nrow(data2), 100)

  data2 <- audioblast("data", "recordings", max_pages = 1, page = 2, source="bio.acousti.ca")
  expect_false(any(data2$id %in% data$id))

  data2 <- audioblast("data", "recordings", max_pages = 1, page_size = 120, source="bio.acousti.ca")
  expect_equal(nrow(data2), 120)

  data2 <- audioblast("data", "recordings", source="bio.acousti.ca", id=c(10754, 11096))
  expect_setequal(data2$id, c("10754", "11096"))

  data2 <- audioblast("data", "recordings", source="bio.acousti.ca", duration=ab_range(10, 20), max_pages=1)
  expect_true(all(data2$duration >= 10 & data2$duration <= 20))

  expect_error(
    audioblast("data", "recordings", source="bio.acousti.ca", bogus=1),
    "Parameter `bogus` is not recognised"
  )

  data <- audioblast("standalone", "analysis", "fetch_analysis_counts", source="unp", id="nhm-unp-1-1588606870")
  expect_equal(names(data), c("counts", "total"))

  expect_true(is.null(audioblast("data", "recordings", source="unp", deployment="horsefly")))

  cl <- audioblast("data", "taxa", "classification", source="bio.acousti.ca", id=247)
  expect_equal(cl$taxon[nrow(cl)], "Gryllotalpa vineae")
  expect_equal(cl$rank[1], "Kingdom")

  ttl <- audioblast("data", "recordings", source="bio.acousti.ca", output="Turtle", max_pages=2, page_size=2)
  expect_length(ttl, 2)
  expect_match(ttl[1], "@prefix", fixed=TRUE)
})

test_that("audioblastRecord works with real data", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  r <- audioblastRecord("recording", "bio.acousti.ca", 10754)
  expect_equal(nrow(r), 1)
  expect_equal(r$id, "10754")
  expect_type(r$duration, "double")

  expect_null(audioblastRecord("recording", "bio.acousti.ca", 99999999))
  expect_match(audioblastRecord("taxon", "bio.acousti.ca", 247, output="Turtle"), "@prefix", fixed=TRUE)
  expect_error(audioblastRecord("koala", "bio.acousti.ca", 1))
})

test_that("audioblastFilters works with real data", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  f <- audioblastFilters("recordings")
  expect_true(all(c("source", "id", "taxon", "duration") %in% f$name))
  expect_false("output" %in% f$name)
  expect_true(f$multiple[f$name == "id"])
  expect_equal(f$type[f$name == "duration"], "range")
  expect_error(audioblastFilters("chipmunk"), "chipmunk module does not exist.")
})

test_that("audioblastDownload works as expected", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  dir <- file.path(tempdir(), "ab_dl_test")
  unlink(dir, recursive=TRUE)
  recs <- audioblast("data", "recordings", source="inaturalist", id="134")

  paths <- audioblastDownload(recs, dir=dir, metadata = FALSE, quiet=TRUE)
  expect_true(dir.exists(dir))
  expect_equal(basename(paths), "134.m4a")
  expect_equal(file.size(paths), 615376)
  expect_equal(length(list.files(dir, pattern="\\.csv$")), 0)
  expect_false(file.exists(paste0(paths, ".part")))

  # Existing files in dir are skipped, not downloaded again
  mtime <- file.mtime(paths)
  audioblastDownload(recs, dir=dir, metadata = TRUE, quiet=TRUE)
  expect_equal(file.mtime(paths), mtime)
  expect_equal(length(list.files(dir, pattern="\\.m4a$")), 1)
  expect_equal(length(list.files(dir, pattern="\\.csv$")), 1)

  unlink(dir, recursive=TRUE)
})

test_that("audioblastDownload names and checks files", {
  dir <- file.path(tempdir(), "ab_hash_test")
  unlink(dir, recursive=TRUE)
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
  unlink(dir, recursive=TRUE)
})

test_that("A failed download leaves no file behind", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code=403))
  path <- file.path(tempdir(), "ab_failed.wav")
  expect_error(.audioblast_download("https://example.org/a.wav", path), "403")
  expect_false(file.exists(path))
  expect_false(file.exists(paste0(path, ".part")))
})

test_that("Annotation output is as expected", {
  skip_on_cran()
  skip_if_offline("api.audioblast.org")
  # Get annotations from audioblast as data.frame
  data <- audioblast("data", "annomate", max_pages = 1, source="bio.acousti.ca")
  expect_true(is.data.frame(data))

  # Get annotations from audioblast as Annotation objects
  anns <- audioblast("data", "annomate", max_pages = 1, source="bio.acousti.ca", output="Annotations")
  expect_true(all(sapply(anns, inherits, "Annotation")))

  d <- data[2,]
  a <- anns[[2]]
  expect_equal(as.numeric(d$time_start), a@start)
  expect_equal(as.numeric(d$time_end), a@end)
  expect_equal(d$source, a@metadata$source)

  anns <- audioblast("data", "annomate", source="bio.acousti.ca", source_id=10754, output="Annotations")
  expect_equal(vapply(anns, function(a) a@end, numeric(1)), c(4, 23))
})
