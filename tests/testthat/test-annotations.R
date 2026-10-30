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

  a1 <- annotation(start=0, end=20, low=5, high=30)
  a2 <- annotation(start=15, end=25, low=0, high=10)
  expect_true(.annotation_check_overlap(a1, a2, domain="both"))
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

test_that("merge_annotations works as expected",{
  a1 <- list(
    annotation(start=0, end=20),
    annotation(start=30, end=40),
    annotation(start=10, end=30)
  )

  test <- list(
    annotation(start=0, end=40)
  )

  expect_true(all.equal(merge_annotations(a1), test))

  a2 <- list(
    annotation(start=0, end=20),
    annotation(start=30, end=40),
    annotation(start=10, end=30),
    annotation(start=5, end=25)
  )

  test <- list(
    annotation(start=0, end=40)
  )

  expect_true(all.equal(merge_annotations(a2), test))

  a3 <- list(
    annotation(low=0, high=10),
    annotation(low=20, high=40),
    annotation(low=10, high=20)
  )

  test <- list(
    annotation(low=0, high=40)
  )

  expect_true(all.equal(merge_annotations(a3, domain="frequency"), test))

  a4 <- list(
    annotation(start=0, end=20, source="a"),
    annotation(start=30, end=40, source="a"),
    annotation(start=10, end=30,  source="a")
  )

  test <- list(
    annotation(start=0, end=40, source="a")
  )

  expect_true(all.equal(merge_annotations(a4), test))

  a5 <- list(
    annotation(start=0, end=20, source="a"),
    annotation(start=30, end=40, source="a"),
    annotation(start=10, end=30,  source="a"),
    annotation(start=0, end=20, source="b"),
    annotation(start=30, end=40, source="b"),
    annotation(start=10, end=30,  source="b")
  )

  test <- list(
    annotation(start=0, end=40, source="a"),
    annotation(start=0, end=40, source="b")
  )

  expect_true(all.equal(merge_annotations(a5), test))

  expect_true(all.equal(merge_annotations(a5, same.source=F), list(test[[2]])))

  a5 <- list(
    annotation(start=0, end=20, file="a"),
    annotation(start=30, end=40, file="a"),
    annotation(start=10, end=30,  file="a"),
    annotation(start=0, end=20, file="b"),
    annotation(start=30, end=40, file="b"),
    annotation(start=10, end=30,  file="b")
  )

  test <- list(
    annotation(start=0, end=40, file="a"),
    annotation(start=0, end=40, file="b")
  )

  expect_true(all.equal(merge_annotations(a5), test))

  a6 <- list(
    annotation(start=0, end=20, file="a", type="monkey"),
    annotation(start=30, end=40, file="a", type="monkey"),
    annotation(start=10, end=30,  file="a", type="monkey"),
    annotation(start=0, end=20, file="b", type="monkey"),
    annotation(start=30, end=40, file="b", type="monkey"),
    annotation(start=10, end=30,  file="b", type="monkey"),
    annotation(start=0, end=20, file="a", type="dog"),
    annotation(start=30, end=40, file="a", type="dog"),
    annotation(start=10, end=30,  file="a", type="dog"),
    annotation(start=0, end=20, file="b", type="dog"),
    annotation(start=30, end=40, file="b", type="dog"),
    annotation(start=10, end=30,  file="b", type="dog")
  )

  test <- list(
    annotation(start=0, end=40, file="a", type="monkey"),
    annotation(start=0, end=40, file="b", type="monkey"),
    annotation(start=0, end=40, file="a", type="dog"),
    annotation(start=0, end=40, file="b", type="dog")
  )

  expect_true(all.equal(merge_annotations(a6), test))


  a7 <- list(
    annotation(start=0, end=20, file="a", type="monkey", value=TRUE),
    annotation(start=30, end=40, file="a", type="monkey", value=TRUE),
    annotation(start=10, end=30,  file="a", type="monkey", value=TRUE),
    annotation(start=0, end=20, file="b", type="monkey", value=TRUE),
    annotation(start=30, end=40, file="b", type="monkey", value=TRUE),
    annotation(start=10, end=30,  file="b", type="monkey", value=TRUE),
    annotation(start=0, end=20, file="a", type="dog", value=TRUE),
    annotation(start=30, end=40, file="a", type="dog", value=TRUE),
    annotation(start=10, end=30,  file="a", type="dog", value=TRUE),
    annotation(start=0, end=20, file="b", type="dog", value=TRUE),
    annotation(start=30, end=40, file="b", type="dog", value=TRUE),
    annotation(start=10, end=30,  file="b", type="dog", value=TRUE),
    annotation(start=0, end=20, file="a", type="monkey", value=FALSE),
    annotation(start=30, end=40, file="a", type="monkey", value=FALSE),
    annotation(start=10, end=30,  file="a", type="monkey", value=FALSE),
    annotation(start=0, end=20, file="b", type="monkey", value=FALSE),
    annotation(start=30, end=40, file="b", type="monkey", value=FALSE),
    annotation(start=10, end=30,  file="b", type="monkey", value=FALSE),
    annotation(start=0, end=20, file="a", type="dog", value=FALSE),
    annotation(start=30, end=40, file="a", type="dog", value=FALSE),
    annotation(start=10, end=30,  file="a", type="dog", value=FALSE),
    annotation(start=0, end=20, file="b", type="dog", value=FALSE),
    annotation(start=30, end=40, file="b", type="dog", value=FALSE),
    annotation(start=10, end=30,  file="b", type="dog", value=FALSE)
  )

  test <- list(
    annotation(start=0, end=40, file="a", type="monkey", value=TRUE),
    annotation(start=0, end=40, file="b", type="monkey", value=TRUE),
    annotation(start=0, end=40, file="a", type="dog", value=TRUE),
    annotation(start=0, end=40, file="b", type="dog", value=TRUE),
    annotation(start=0, end=40, file="a", type="monkey", value=FALSE),
    annotation(start=0, end=40, file="b", type="monkey", value=FALSE),
    annotation(start=0, end=40, file="a", type="dog", value=FALSE),
    annotation(start=0, end=40, file="b", type="dog", value=FALSE)
  )

  expect_true(all.equal(merge_annotations(a7), test))
})

test_that("an annotation running past the end of the file is cut to the file", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  wave <- readWave(f)
  d <- seewave::duration(wave)
  a <- annotation(f, start=0, end=d + 10)

  #The whole file is written rather than an error being raised, and the reason
  #is said out loud.
  expect_message(writeAnnotationWave(a), "greater than the duration")
  out <- paste0("AUDIOMOTH_0-", d + 10, ".wav")
  withr::defer(unlink(out))
  expect_true(file.exists(out))
  expect_equal(seewave::duration(readWave(out)), d, tolerance=0.01)
})

test_that("merging no annotations gives no annotations", {
  expect_equal(merge_annotations(list()), list())
  expect_length(merge_annotations(list()), 0)
})

test_that("a single annotation is returned as it is, in either domain", {
  a <- annotation(start=0, end=10, low=100, high=200)
  expect_equal(.merge_annotations(list(a)), list(a))
  expect_equal(.merge_annotations(list(a), domain="frequency"), list(a))
  expect_equal(.merge_annotations(list()), list())
})

test_that("merging in a domain the package does not know is refused by name", {
  a <- annotation(start=0, end=10)
  b <- annotation(start=5, end=15)
  expect_error(
    .merge_annotations(list(a, b), domain="amplitude"),
    "Unknown domain for merge_annotations: amplitude"
  )
})

test_that("overlapping annotations are merged in the frequency domain", {
  a <- annotation(low=100, high=300)
  b <- annotation(low=200, high=500)
  c <- annotation(low=900, high=1000)
  merged <- .merge_annotations(list(a, b, c), domain="frequency")
  expect_length(merged, 2)
  expect_equal(merged[[1]]@low, 100)
  expect_equal(merged[[1]]@high, 500)
  expect_equal(merged[[2]]@low, 900)
})

test_that("annotations whose value is NA are merged with each other and nothing else", {
  # Compared with ==, NA gave NA, which took both annotations out of every
  # group and left NULL in their place, so merging failed on a slot of NULL.
  x <- annotation(start=0, end=1, value="x")
  a <- annotation(start=0, end=1, value=NA)
  b <- annotation(start=0.5, end=2, value=NA)
  text <- annotation(start=1.5, end=3, value="NA")
  merged <- merge_annotations(list(x, a, b, text))
  expect_length(merged, 3)
  expect_identical(merged[[1]]@value, "x")
  expect_identical(merged[[2]]@value, NA_character_)
  expect_equal(c(merged[[2]]@start, merged[[2]]@end), c(0, 2))
  expect_identical(merged[[3]]@value, "NA")

  # The same holds of the file, the type and the source.
  merged <- merge_annotations(list(
    annotation(start=0, end=1, file=NA, type=NA, source=NA),
    annotation(start=0.5, end=2, file=NA, type=NA, source=NA)
  ))
  expect_length(merged, 1)
  expect_equal(merged[[1]]@end, 2)
})

test_that("an annotation whose limits are not known overlaps nothing", {
  unknown <- annotation(start=NA, end=NA, low=NA, high=NA)
  known <- annotation(start=0, end=1, low=0, high=100)
  for (domain in c("time", "frequency", "both")) {
    expect_false(.annotation_check_overlap(unknown, known, domain=domain))
    expect_false(.annotation_check_overlap(known, unknown, domain=domain))
  }
  merged <- merge_annotations(list(unknown, known, annotation(start=0.5, end=2)))
  expect_length(merged, 2)
  expect_equal(c(merged[[1]]@start, merged[[1]]@end), c(0, 2))
  expect_identical(merged[[2]]@start, NA_real_)
  merged <- merge_annotations(list(unknown, known, annotation(low=50, high=200)), domain="frequency")
  expect_length(merged, 2)
})

test_that("an overlap in a domain the package does not know is refused", {
  a <- annotation(start=0, end=10)
  expect_error(.annotation_check_overlap(a, a, domain="amplitude"), "Unknown domain: amplitude")
})

test_that("a merged annotation spans those merged into it in both domains", {
  # Merging in time took the frequency limits of the later annotation alone.
  wide <- annotation(start=0, end=2, low=1000, high=5000)
  narrow <- annotation(start=1, end=3, low=2000, high=3000)
  merged <- merge_annotations(list(wide, narrow))
  expect_length(merged, 1)
  expect_equal(c(merged[[1]]@start, merged[[1]]@end, merged[[1]]@low, merged[[1]]@high),
               c(0, 3, 1000, 5000))

  # Merging in frequency took the times of the later annotation alone.
  early <- annotation(start=0, end=1, low=100, high=300)
  late <- annotation(start=10, end=11, low=200, high=500)
  merged <- merge_annotations(list(late, early), domain="frequency")
  expect_length(merged, 1)
  expect_equal(c(merged[[1]]@start, merged[[1]]@end, merged[[1]]@low, merged[[1]]@high),
               c(0, 11, 100, 500))
})

test_that("a merged annotation keeps the metadata of the last annotation merged into it", {
  first <- annotation(start=27, end=30, source="a", metadata=list(confidence=0.9))
  second <- annotation(start=30, end=33, source="b", metadata=list(confidence=0.8))
  merged <- merge_annotations(list(second, first), same.source=FALSE)
  expect_length(merged, 1)
  expect_identical(merged[[1]]@metadata, list(confidence=0.8))
  expect_identical(merged[[1]]@source, "b")
})

test_that("merging keeps the names a list of annotations gives them", {
  a <- list(first=annotation(start=0, end=1), second=annotation(start=5, end=6, value="v"))
  expect_identical(names(merge_annotations(a)), c("first", "second"))
  expect_null(names(merge_annotations(unname(a))))
})

test_that("sorting in a domain the package does not know is refused", {
  # It returned the annotations unsorted, without saying so.
  a <- list(annotation(start=2), annotation(start=1))
  expect_error(sort_annotations(a, domain="freq"), "Unknown domain for sort_annotations: freq")
})

test_that("a list holding something other than an annotation is refused", {
  a <- annotation(start=0, end=10)
  expect_error(
    AnnotationList2DataFrame(list(a, "not an annotation")),
    "All items in list must be Annotation objects."
  )
  expect_error(AnnotationList2DataFrame(list(1, 2)), "must be Annotation objects")
})

test_that("metadata keeps its class in a data frame", {
  # Every value was written into a column of text, so a confidence came back
  # as "0.9".
  x <- list(
    annotation(start=0, end=1, metadata=list(confidence=0.9, name="a", when=as.Date("2024-06-30"))),
    annotation(start=1, end=2, metadata=list(confidence=0.8)),
    annotation(start=2, end=3, metadata=list(name="c"))
  )
  d <- AnnotationList2DataFrame(x)
  expect_identical(d$start, c(0, 1, 2))
  expect_identical(d$confidence, c(0.9, 0.8, NA))
  expect_identical(d$name, c("a", NA, "c"))
  expect_identical(d$when, as.Date(c("2024-06-30", NA, NA)))
})

test_that("a plain NA among values of one class keeps that class", {
  d <- AnnotationList2DataFrame(list(
    annotation(metadata=list(confidence=NA, when=NA)),
    annotation(metadata=list(confidence=0.8, when=as.Date("2024-06-30")))
  ))
  expect_identical(d$confidence, c(NA, 0.8))
  expect_identical(d$when, as.Date(c(NA, "2024-06-30")))
  # A column of TRUE, FALSE and NA stays logical.
  d <- AnnotationList2DataFrame(list(
    annotation(metadata=list(flag=NA)), annotation(metadata=list(flag=TRUE))
  ))
  expect_identical(d$flag, c(NA, TRUE))
})

test_that("metadata of differing classes, or of more than one value, is kept", {
  d <- AnnotationList2DataFrame(list(
    annotation(metadata=list(id=1, tags=c("a", "b"))),
    annotation(metadata=list(id="two", tags="c"))
  ))
  expect_identical(nrow(d), 2L)
  expect_identical(d$id, c("1", "two"))
  expect_true(is.list(d$tags))
  expect_identical(d$tags[[1]], c("a", "b"))
  expect_identical(d$tags[[2]], "c")
})

test_that("metadata that is NULL wherever it is given is NA", {
  d <- AnnotationList2DataFrame(list(annotation(metadata=list(empty=NULL, 1))))
  expect_identical(d$empty, NA)
  # A value with no name has nothing to name a column by, and is left out.
  expect_identical(
    colnames(d),
    c("file", "start", "end", "low", "high", "source", "type", "value", "empty")
  )
})

test_that("a metadata name that is also a slot is given a column of its own", {
  # audioblast() keeps the collection an annotation came from as metadata named
  # source. That made a second column named source, and its value was written
  # over the slot's in the first.
  d <- AnnotationList2DataFrame(list(
    annotation(source="audioblast", metadata=list(source="sounds_of_norway"))
  ))
  expect_identical(d$source, "audioblast")
  expect_identical(d$metadata.source, "sounds_of_norway")
  expect_identical(anyDuplicated(colnames(d)), 0L)

  d <- AnnotationList2DataFrame(list(
    annotation(source="a", metadata=list(source="b", metadata.source="c"))
  ))
  expect_identical(anyDuplicated(colnames(d)), 0L)
  expect_identical(d$source, "a")
  expect_setequal(unlist(d[1, -(1:8)]), c("b", "c"))
})

test_that("no annotations give a data frame of no rows", {
  d <- AnnotationList2DataFrame(list())
  expect_identical(nrow(d), 0L)
  expect_identical(d$start, numeric(0))
  expect_identical(d$value, character(0))
})

test_that("an annotation that ends before it starts is refused when it is made", {
  # It was made without complaint and merged into nonsense later.
  expect_error(annotation(start=2, end=1), "start \\(2\\) is after end \\(1\\)")
  expect_error(annotation(low=2000, high=1000), "low \\(2000\\) is above high \\(1000\\)")
  expect_error(new("Annotation", start=2, end=1), "is after end")
  expect_error(annotation(start=2, end=1, low=2, high=1), "after end.*above high")
})

test_that("each slot but metadata holds a single value", {
  # Two starts failed later, in sorting, with "unimplemented type 'list'".
  expect_error(annotation(start=c(0, 1), end=2), "start must be a single value, not 2 values")
  expect_error(annotation(value=NULL), "value must be a single value, not 0 values")
  expect_error(annotation(file=c("a", "b"), type=character(0)),
               "file must be a single value.*type must be a single value")
  expect_s4_class(annotation(metadata=list(a=1:3, b="x")), "Annotation")
})

test_that("a limit that is not known, or a point in time, is still an annotation", {
  expect_s4_class(annotation(start=NA, end=NA, low=NA, high=NA), "Annotation")
  expect_s4_class(annotation(start=1, end=NA), "Annotation")
  expect_s4_class(annotation(start=1, end=1, low=500, high=500), "Annotation")
  expect_s4_class(annotation(), "Annotation")
})

test_that("a single annotation is taken where a list of them is", {
  # Each failed with "no method for coercing this S4 class to a vector".
  a <- annotation(start=0, end=1, value="x", metadata=list(confidence=0.9))
  expect_identical(merge_annotations(a), list(a))
  expect_identical(sort_annotations(a), list(a))
  expect_identical(AnnotationList2DataFrame(a), AnnotationList2DataFrame(list(a)))
  f <- tempfile(fileext=".txt")
  writeAudacityLabels(a, f)
  expect_identical(readLines(f), "0\t1\tx")
})

test_that("a list holding something other than an annotation is refused by name", {
  a <- annotation(start=0, end=1)
  msg <- "All items in list must be Annotation objects."
  expect_error(merge_annotations(list(a, "b")), msg, fixed=TRUE)
  expect_error(sort_annotations(list(a, 1)), msg, fixed=TRUE)
  expect_error(writeAudacityLabels(list(a, 1), tempfile()), msg, fixed=TRUE)
  expect_error(writeAnnotationWave(list(a, 1), tuneR::sine(440)), msg, fixed=TRUE)
  expect_error(AnnotationList2DataFrame(data.frame(start=1)), msg, fixed=TRUE)
  expect_error(merge_annotations("a"), msg, fixed=TRUE)
})
