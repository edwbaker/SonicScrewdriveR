test_that("ASITSN rejects unknown input", {
  skip_on_cran()
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
  skip_on_cran()
  expect_error(
    audioblast("data", "recordings", source="bio.acousti.ca", output="koala"),
    "koala is not a valid output type."
  )
  expect_error(
    audioblast("data", "recordings", max_pages=1, output="Annotations"),
    "Query does not gives results that can be turned into Annotation objects."
  )
  expect_silent(
    audioblast("data", "annomate", max_pages=1, on.issue=invsible, output="Annotations")
  )
})

test_that("audioblast works with real data", {
  skip_on_cran()
  data <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca")
  expect_equal(nrow(data), 50)

  data2 <- audioblast("data", "recordings", max_pages = 2, source="bio.acousti.ca", quiet=TRUE)
  expect_equal(nrow(data2), 100)

  data2 <- audioblast("data", "recordings", max_pages = 1, page = 2, source="bio.acousti.ca")
  expect_false(any(data2$id %in% data$id))

  data <- audioblast("standalone", "analysis", "fetch_analysis_counts", source="unp", id="nhm-unp-1-1588606870", )
  expect_equal(names(data), c("counts", "total"))

  expect_true(is.null(audioblast("data", "recordings", source="unp", deployment="horsefly", )))
})

test_that("audioblastDownload works as expected", {
  skip_on_cran()
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
  skip_on_cran()
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
