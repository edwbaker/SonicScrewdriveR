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


test_that("audioblast rejects unknown output", {
  local_audioblast()
  expect_error(
    audioblast("data", "recordings", source="bio.acousti.ca", output="koala"),
    "koala is not a valid output type."
  )
  expect_error(
    audioblast("data", "recordings", max_pages=1, output="Annotations"),
    "Query does not gives results that can be turned into Annotation objects."
  )
  expect_silent(
    audioblast("data", "annomate", max_pages=1, on.issue=invisible, output="Annotations")
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

test_that("audioblastDownload works as expected", {
  local_audioblast()
  local_mocked_bindings(download.file=.audioblastDownload)
  if (dir.exists("ab_dl_test")) {
    unlink("ab_dl_test", recursive=TRUE)
  }
  recs <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca", id="11096", )

  audioblastDownload(recs, dir="ab_dl_test", metadata = FALSE, quiet=TRUE, )
  expect_true(dir.exists("ab_dl_test"))
  expect_equal(length(list.files("ab_dl_test", pattern="*.wav")), 1)
  expect_equal(length(list.files("ab_dl_test", pattern="*.csv")), 0)

  audioblastDownload(recs, dir="ab_dl_test", metadata = TRUE, quiet=TRUE, )
  expect_equal(length(list.files("ab_dl_test", pattern="*.wav")), 1)
  expect_equal(length(list.files("ab_dl_test", pattern="*.csv")), 1)

  if (dir.exists("ab_dl_test")) {
    unlink("ab_dl_test", recursive=TRUE)
  }
})

test_that("Annotation output is as expected", {
  local_audioblast()
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
})

#The pages below never leave the machine: fromJSON() is stood in for, so what is
#checked is which pages are asked for and how they are joined together.
pagedApi <- function(last_page=3, env=parent.frame()) {
  asked <- new.env(parent=emptyenv())
  asked$urls <- character(0)
  testthat::local_mocked_bindings(
    fromJSON = function(txt, ...) {
      asked$urls <- c(asked$urls, txt)
      page <- as.integer(regmatches(txt, regexpr("(?<=page=)[0-9]+", txt, perl=TRUE)))
      list(
        data = data.frame(page=page, taxon=paste("taxon", page)),
        last_page = last_page
      )
    },
    .package = "jsonlite",
    .env = env
  )
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

test_that("the endpoint asked for is kept on every page, not just the first", {
  api <- pagedApi()
  audioblast("data", "annomate", endpoint="taxa", check=FALSE, quiet=TRUE)
  expect_length(api$urls, 3)
  expect_true(all(grepl("/annomate/taxa/", api$urls, fixed=TRUE)))
  expect_equal(api$urls[1], "https://api.audioblast.org/data/annomate/taxa/?page=1")
})

test_that("the fields filtered on are kept on every page", {
  api <- pagedApi()
  audioblast("data", "recordings", check=FALSE, quiet=TRUE, taxon="Gryllotalpa")
  expect_length(api$urls, 3)
  expect_true(all(grepl("&taxon=Gryllotalpa", api$urls, fixed=TRUE)))
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
  #download.file() is stood in for so that nothing is fetched.
  attempted <- character(0)
  local_mocked_bindings(
    download.file = function(url, destfile, quiet=FALSE, ...) {
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
