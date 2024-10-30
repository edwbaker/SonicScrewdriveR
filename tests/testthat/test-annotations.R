test_that("annotation overlaps detected time domain", {
  a1 <- annotation(start=0, end=15)
  a2 <- annotation(start=20, end=30)
  a3 <- annotation(start=5, end=10)
  a4 <- annotation(start=10, end=30)
  a5 <- annotation(start=15, end=30)

  expect_false(.annotation_check_overlap(a1, a2))
  expect_false(.annotation_check_overlap(a2, a1))
  expect_true(.annotation_check_overlap(a1, a3))
  expect_true(.annotation_check_overlap(a3, a1))
  expect_true(.annotation_check_overlap(a1, a4))
  expect_true(.annotation_check_overlap(a4, a1))
  expect_true(.annotation_check_overlap(a1, a5))
  expect_true(.annotation_check_overlap(a5, a1))
})

test_that("annotation overlaps detected frequency domain", {
  a1 <- annotation(low=0, high=1000)
  a2 <- annotation(low=2000, high=3000)
  a3 <- annotation(low=500, high=1500)
  a4 <- annotation(low=200, high=800)
  a5 <- annotation(low=1000, high=2000)

  expect_false(.annotation_check_overlap(a1, a2, domain="frequency"))
  expect_false(.annotation_check_overlap(a2, a1, domain="frequency"))
  expect_true(.annotation_check_overlap(a1, a3, domain="frequency"))
  expect_true(.annotation_check_overlap(a3, a1, domain="frequency"))
  expect_true(.annotation_check_overlap(a1, a4, domain="frequency"))
  expect_true(.annotation_check_overlap(a4, a1, domain="frequency"))
  expect_true(.annotation_check_overlap(a1, a5, domain="frequency"))
  expect_true(.annotation_check_overlap(a5, a1, domain="frequency"))
})

test_that("annotation overlaps detected time and frequency domain", {
  a1 <- annotation(start=0, end=10, low=0, high=1000)
  a2 <- annotation(start=20, end=30, low=2000, high=3000)
  a3 <- annotation(start=5, end=15, low=500, high=1500)
  a4 <- annotation(start=0, end=10, low=1000, high=2000)

  expect_false(.annotation_check_overlap(a1, a2, domain="both"))
  expect_false(.annotation_check_overlap(a2, a1, domain="both"))
  expect_true(.annotation_check_overlap(a1, a3, domain="both"))
  expect_true(.annotation_check_overlap(a3, a1, domain="both"))
  expect_true(.annotation_check_overlap(a1, a4, domain="both"))
  expect_true(.annotation_check_overlap(a4, a1, domain="both"))
})

test_that("merging overlapping annotations works", {
  a1 <- annotation(start=0, end=10)
  a2 <- annotation(start=5, end=15)
  a3 <- annotation(start=0, end=15)

  expect_true(all.equal(.annotation_merge_overlapping(a1, a2), a3))

  a1 <- annotation(low=0, high=1000)
  a2 <- annotation(low=500, high=1500)
  a3 <- annotation(low=0, high=1500)

  expect_true(all.equal(.annotation_merge_overlapping(a1, a2, domain="frequency"), a3))

  a4 <- annotation(start=0, end=10, low=0, high=1000)
  a5 <- annotation(start=5, end=20, low=500, high=1500)
  a6 <- annotation(start=0, end=20, low=0, high=1500)

  expect_true(all.equal(.annotation_merge_overlapping(a4, a5, domain="both"), a6))

  a7 <- annotation(start=0, end=10, low=0, high=1000)
  a8 <- annotation(start=50, end=200, low=5000, high=15000)

  expect_false(.annotation_merge_overlapping(a7, a8, domain="both"))
})

test_that("sort_annotations works", {
  a <- list(
    annotation(start=10, end=10, low=30, high=40),
    annotation(start=5, end=15, low=20, high=30),
    annotation(start=0, end=15, low=10, high=20)
  )

  test <- list(
    annotation(start=0, end=15, low=10, high=20),
    annotation(start=5, end=15, low=20, high=30),
    annotation(start=10, end=10, low=30, high=40)
  )

  expect_true(all.equal(sort_annotations(a), test))
  expect_true(all.equal(sort_annotations(a, domain="frequency"), test))
  expect_true(all.equal(sort_annotations(test, decreasing=TRUE), rev(test)))
  expect_true(all.equal(sort_annotations(test, domain="frequency", decreasing=TRUE), rev(test)))

  a <- list(
    annotation(start=10, end=20, low=0, high=10),
    annotation(start=0, end=15, low=10, high=20),
    annotation(start=10, end=20, low=20, high=30)
  )

  test <- list(
    annotation(start=0, end=15, low=10, high=20),
    annotation(start=10, end=20, low=0, high=10),
    annotation(start=10, end=20, low=20, high=30)
  )

  expect_true(all.equal(sort_annotations(a, domain="both"), test))
})

test_that("annotations_merge works as expected",{
  a1 <- list(
    annotation(start=0, end=20),
    annotation(start=30, end=40),
    annotation(start=10, end=30)
  )

  test <- list(
    annotation(start=0, end=40)
  )

  expect_true(all.equal(annotations_merge(a1), test))

  a2 <- list(
    annotation(start=0, end=20),
    annotation(start=30, end=40),
    annotation(start=10, end=30),
    annotation(start=5, end=25)
  )

  test <- list(
    annotation(start=0, end=40)
  )

  expect_true(all.equal(annotations_merge(a2), test))

  a3 <- list(
    annotation(low=0, high=10),
    annotation(low=20, high=40),
    annotation(low=10, high=20)
  )

  test <- list(
    annotation(low=0, high=40)
  )

  expect_true(all.equal(annotations_merge(a3, domain="frequency"), test))

  a4 <- list(
    annotation(start=0, end=15, low=10, high=20),
    annotation(start=10, end=20, low=5, high=30),
    annotation(start=15, end=25, low=0, high=10)
  )

  test <- list(
    annotation(start=0, end=25, low=0, high=30)
  )

  expect_true(all.equal(annotations_merge(a4, domain="both"), test))

  a5 <- list(
    annotation(start=0, end=15, low=10, high=20),
    annotation(start=10, end=20, low=5, high=30),
    annotation(start=10, end=20, low=70, high=80),
    annotation(start=15, end=25, low=0, high=10)
  )

  test <- list(
    annotation(start=0, end=25, low=0, high=30),
    annotation(start=10, end=20, low=70, high=80)
  )

  expect_true(all.equal(annotations_merge(a5, domain="both"), test))
})
