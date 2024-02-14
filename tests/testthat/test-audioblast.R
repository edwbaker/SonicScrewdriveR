test_that("ASITSN rejects unknown input", {
  expect_error(.audioblast_ASITSN("standalone", "chipmunk"), "chipmunk module does not exist.")
  expect_error(.audioblast_ASITSN("data", "recordings", "ground hog"), "ground hog is not a valid endpoint.")
})

test_that("audioblast works with real data", {
  data <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca")
  expect_equal(nrow(data), 50)

  data2 <- audioblast("data", "recordings", max_pages = 2, source="bio.acousti.ca")
  expect_equal(nrow(data2), 100)

  data2 <- audioblast("data", "recordings", max_pages = 1, page = 2, source="bio.acousti.ca")
  expect_false(any(data2$id %in% data$id))

  data <- audioblast("standalone", "analysis", "fetch_analysis_counts", source="unp", id="nhm-unp-1-1588606870")
  expect_equal(names(data), c("counts", "total"))

  expect_true(is.null(audioblast("data", "recordings", source="unp", deployment="horsefly")))
})

test_that("audioblastDownload works as expected", {
  if (dir.exists("ab_dl_test")) {
    unlink("ab_dl_test", recursive=TRUE)
  }
  recs <- audioblast("data", "recordings", max_pages = 1, source="bio.acousti.ca", id="11096")

  audioblastDownload(recs, dir="ab_dl_test", metadata = FALSE)
  expect_true(dir.exists("ab_dl_test"))
  expect_equal(length(list.files("ab_dl_test", pattern="*.wav")), 1)
  expect_equal(length(list.files("ab_dl_test", pattern="*.csv")), 0)

  audioblastDownload(recs, dir="ab_dl_test", metadata = TRUE)
  expect_equal(length(list.files("ab_dl_test", pattern="*.wav")), 1)
  expect_equal(length(list.files("ab_dl_test", pattern="*.csv")), 1)

  if (dir.exists("ab_dl_test")) {
    unlink("ab_dl_test", recursive=TRUE)
  }
})
