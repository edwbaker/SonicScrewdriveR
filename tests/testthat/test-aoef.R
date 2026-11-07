aoefFixture <- function(name) {
  test_path("fixtures", "aoef", paste0(name, ".json"))
}

#Writes a small AOEF document given as R lists, for the cases no file covers.
aoefDocument <- function(data, version="1.1.0") {
  f <- withr::local_tempfile(fileext=".json", .local_envir=parent.frame())
  writeLines(jsonlite::toJSON(list(version=version, data=data), auto_unbox=TRUE, null="null"), f)
  return(f)
}

aoefMinimal <- function(...) {
  data <- list(
    uuid="00000000-0000-4000-8000-000000000000",
    collection_type="annotation_set",
    tags=list(list(id=0, key="species", value="Turdus merula")),
    recordings=list(list(uuid="00000000-0000-4000-8000-000000000001", path="a.wav",
                         duration=10, channels=1, samplerate=48000)),
    clips=list(list(uuid="00000000-0000-4000-8000-000000000002",
                    recording="00000000-0000-4000-8000-000000000001", start_time=0, end_time=10)),
    sound_events=list(list(uuid="00000000-0000-4000-8000-000000000003",
                           recording="00000000-0000-4000-8000-000000000001",
                           geometry=list(type="TimeInterval", coordinates=list(1, 2)))),
    sound_event_annotations=list(list(uuid="00000000-0000-4000-8000-000000000004",
                                      sound_event="00000000-0000-4000-8000-000000000003",
                                      tags=list(0))),
    clip_annotations=list(list(uuid="00000000-0000-4000-8000-000000000005",
                               clip="00000000-0000-4000-8000-000000000002",
                               sound_events=list("00000000-0000-4000-8000-000000000004")))
  )
  changes <- list(...)
  data[names(changes)] <- changes
  return(data)
}

#The parsed JSON of a written file.
aoefJSON <- function(f) {
  return(jsonlite::parse_json(paste(readLines(f, warn=FALSE), collapse="")))
}

test_that("readAOEF reads every shape soundevent has as the range holding it", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"))
  expect_length(a, 13)
  expect_true(all(vapply(a, inherits, logical(1), "Annotation")))
  #Made by filling in slots rather than by annotation(), so checked here as
  #annotation() checks them.
  for (name in c("soundevent_annotation_project", "soundevent_example_evaluation_set",
                 "soundevent_example_model_run", "soundevent_nips4b_plus_sample")) {
    expect_silent(lapply(readAOEF(aoefFixture(name)), validObject))
  }
  df <- AnnotationList2DataFrame(a)
  shapes <- vapply(a, function(x) {
    g <- x@metadata$aoef$geometry
    if (is.null(g)) "clip" else g$type
  }, character(1))
  expect_equal(shapes, c("clip", "BoundingBox", "BoundingBox", "TimeInterval", "TimeStamp", "Point",
                         "LineString", "Polygon", "TimeInterval", "MultiPoint", "MultiLineString",
                         "MultiPolygon", "clip"))
  expect_equal(df$start, c(0, 1, 1, 3, 5.5, 6, 7, 11, 13, 15, 16, 18, 0))
  expect_equal(df$end, c(10, 2.5, 2.5, 4.25, 5.5, 6, 8, 12, 14, 15.5, 17.2, 19.5, 5))
  expect_equal(df$low, c(0, 2000, 2000, 0, 0, 4000, 3000, 1000, 0, 2000, 1000, 500, 0))
  expect_equal(df$high, c(Inf, 6000, 6000, Inf, Inf, 4000, 5000, 3000, Inf, 2500, 2600, 1200, Inf))
  expect_equal(unique(df$source), "readAOEF")
})

test_that("readAOEF reads a sound event as one annotation for each of its tags", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"))
  #soundevent writes the key species as the Darwin Core term it stands for.
  expect_equal(c(a[[2]]@type, a[[2]]@value), c("dwc:scientificName", "Turdus merula"))
  expect_equal(c(a[[3]]@type, a[[3]]@value), c("call_type", "song"))
  expect_identical(a[[2]]@metadata$aoef$uuid, a[[3]]@metadata$aoef$uuid)
  #An event with no tags is read as one annotation with no type or value.
  expect_equal(c(a[[9]]@type, a[[9]]@value), c("", ""))
  expect_equal(a[[5]]@value, "Pip\u00e9strelle")
  expect_equal(Encoding(a[[5]]@value), "UTF-8")
})

test_that("readAOEF reads the tags of clips and clips annotated as holding nothing", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"))
  expect_equal(c(a[[1]]@type, a[[1]]@value), c("quality", "noisy"))
  expect_equal(a[[1]]@metadata$notes, "Wind from 8 s")
  expect_equal(a[[1]]@metadata$aoef$object, "clip_annotation")
  expect_equal(a[[13]]@metadata$aoef$object, "clip_annotation")

  #soundevent's own sample is of clips annotated as holding nothing at all.
  n <- readAOEF(aoefFixture("soundevent_nips4b_plus_sample"))
  expect_length(n, 10)
  expect_equal(unique(vapply(n, function(x) x@value, character(1))), "")
  expect_equal(n[[1]]@file, "train/nips4b_birds_trainfile079.wav")
  expect_equal(n[[1]]@end, 5.00390022675737)
})

test_that("readAOEF keeps who made an annotation, when, its notes and its features", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"))
  expect_equal(a[[2]]@identifiedBy, "Ed Baker")
  #A user with a name and no username is named by the name.
  expect_equal(a[[4]]@identifiedBy, "A. Recordist")
  expect_equal(a[[2]]@dateIdentified, "2026-10-30T12:00:00")
  #Who and when are the annotation's own slots, not its metadata.
  expect_null(a[[2]]@metadata$created_by)
  expect_null(a[[2]]@metadata$created_on)
  expect_equal(a[[2]]@metadata$notes, c("Clear song", "Second phrase overlaps"))
  expect_equal(a[[2]]@metadata$features, c(snr=12.5))
  kept <- a[[2]]@metadata$aoef$recording
  expect_equal(kept$duration, 20)
  expect_equal(kept$samplerate, 48000)
  expect_equal(kept$latitude, 51.4967)
  expect_equal(kept$date, "2026-05-01")
  expect_null(kept$path)
  expect_equal(a[[13]]@metadata$aoef$recording$time_expansion, 10)
})

test_that("readAOEF puts the paths of recordings under audio.dir, with forward slashes", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"))
  #Written on Windows, the paths separate their parts with backslashes.
  expect_equal(a[[1]]@file, "site1/long.wav")
  a <- readAOEF(aoefFixture("soundevent_annotation_project"), audio.dir="D:/audio/")
  expect_equal(a[[1]]@file, "D:/audio/site1/long.wav")
  expect_equal(a[[13]]@file, "D:/audio/site2/short.wav")
  #An absolute path is not put under it.
  e <- readAOEF(aoefFixture("soundevent_example_evaluation_set"), audio.dir="D:/audio")
  expect_equal(e[[1]]@file, "/audio/audio/XC882096-Diclidurus-albus_ind2.wav")
  expect_equal(sonicscrewdriver:::.aoefPath("C:\\x\\y.wav", "D:/audio"), "C:/x/y.wav")
})

test_that("readAOEF gives a data frame", {
  df <- readAOEF(aoefFixture("soundevent_annotation_project"), output="data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 13)
  expect_true(all(c("identifiedBy", "dateIdentified", "notes", "features", "aoef") %in% colnames(df)))
  expect_equal(df$identifiedBy[2], "Ed Baker")
  expect_error(readAOEF(aoefFixture("soundevent_annotation_project"), output="ewe"), "Unknown output format.")
})

test_that("readAOEF reads the annotations of an evaluation set as Whombat writes them", {
  e <- readAOEF(aoefFixture("soundevent_example_evaluation_set"))
  events <- Filter(function(x) x@metadata$aoef$object == "sound_event_annotation", e)
  expect_length(unique(vapply(events, function(x) x@metadata$aoef$uuid, character(1))), 8)
  expect_equal(e[[1]]@value, "Diclidurus albus")
  expect_equal(e[[1]]@start, 2.2304061436545557)
  expect_equal(e[[1]]@identifiedBy, "Santiago Martinez Balvanera")
})

test_that("readAOEF reads predictions with their scores, from the model that made them", {
  m <- readAOEF(aoefFixture("soundevent_example_model_run"))
  expect_length(m, 8)
  #A model run of the older form names its model as the run.
  expect_equal(unique(vapply(m, function(x) x@source, character(1))), "Support Vector Machine for Bats")
  expect_equal(m[[1]]@metadata$score, 1)
  expect_equal(m[[1]]@metadata$tag_score, 1)
  expect_equal(m[[1]]@metadata$aoef$object, "sound_event_prediction")

  #The newer form gives the model a field of its own.
  data <- aoefMinimal(
    collection_type="model_run",
    model=list(name="BirdNET", version="2.4"),
    sound_event_annotations=NULL,
    clip_annotations=NULL,
    tags=list(list(id=0, key="species", value="Turdus merula"), list(id=1, key="species", value="Sylvia")),
    sound_event_predictions=list(list(uuid="00000000-0000-4000-8000-000000000006",
                                      sound_event="00000000-0000-4000-8000-000000000003",
                                      score=0.8, tags=list(list(0, 0.7), list(1, 0.2), list(7, 0.1)))),
    clip_predictions=list(list(uuid="00000000-0000-4000-8000-000000000007",
                               clip="00000000-0000-4000-8000-000000000002",
                               tags=list(list(1, 0.6)), features=list(snr=3)))
  )
  p <- readAOEF(aoefDocument(data))
  expect_length(p, 3)
  expect_equal(p[[1]]@source, "BirdNET")
  expect_equal(p[[1]]@metadata$aoef$object, "clip_prediction")
  expect_equal(p[[1]]@metadata$tag_score, 0.6)
  expect_null(p[[1]]@metadata$score)
  expect_equal(p[[1]]@metadata$features, c(snr=3))
  #A tag the file does not hold is passed over, as soundevent passes over it.
  expect_equal(vapply(p[2:3], function(x) x@value, character(1)), c("Turdus merula", "Sylvia"))
  expect_equal(vapply(p[2:3], function(x) x@metadata$tag_score, numeric(1)), c(0.7, 0.2))
  expect_equal(p[[2]]@metadata$score, 0.8)

  #A model run that names no model is from readAOEF().
  data$model <- NULL
  expect_equal(readAOEF(aoefDocument(data))[[1]]@source, "readAOEF")
})

test_that("readAOEF reads sound events in no clip, which soundevent leaves out", {
  data <- aoefMinimal(clip_annotations=list())
  a <- readAOEF(aoefDocument(data))
  expect_length(a, 1)
  expect_equal(a[[1]]@value, "Turdus merula")
  expect_null(a[[1]]@metadata$aoef$clip)
})

test_that("readAOEF names the person who made an annotation as best it can", {
  users <- list(
    list(uuid="00000000-0000-4000-8000-0000000000a1", username="eb"),
    list(uuid="00000000-0000-4000-8000-0000000000a2", email="a@example.org"),
    list(uuid="00000000-0000-4000-8000-0000000000a3", name="")
  )
  context <- list(users=sonicscrewdriver:::.aoefIndex(users, "uuid"))
  name <- function(id) sonicscrewdriver:::.aoefUserName(context$users, id)
  expect_equal(name("00000000-0000-4000-8000-0000000000a1"), "eb")
  expect_equal(name("00000000-0000-4000-8000-0000000000a2"), "a@example.org")
  expect_equal(name("00000000-0000-4000-8000-0000000000a3"), "00000000-0000-4000-8000-0000000000a3")
  #A user the file does not list is named by their identifier.
  expect_equal(name("00000000-0000-4000-8000-0000000000ff"), "00000000-0000-4000-8000-0000000000ff")
  expect_null(name(NULL))
})

test_that("readAOEF reads a file that is not UTF-8, or begins with a byte order mark", {
  text <- paste(readLines(aoefFixture("soundevent_annotation_project"), encoding="UTF-8", warn=FALSE), collapse="")
  #soundevent on Windows writes in the encoding of the locale.
  latin <- withr::local_tempfile(fileext=".json")
  writeBin(iconv(text, from="UTF-8", to="CP1252", toRaw=TRUE)[[1]], latin)
  expect_equal(readAOEF(latin)[[5]]@value, "Pip\u00e9strelle")

  bom <- withr::local_tempfile(fileext=".json")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8(text))), bom)
  expect_equal(readAOEF(bom)[[5]]@value, "Pip\u00e9strelle")
})

test_that("readAOEF warns of a version it does not read, and refuses what is not AOEF", {
  expect_warning(readAOEF(aoefDocument(aoefMinimal(), version="1.0.0")), "version 1.0.0 of AOEF")
  expect_warning(readAOEF(aoefDocument(aoefMinimal(), version=NULL)), "in no version of AOEF")
  expect_error(readAOEF(tempfile(fileext=".json")), "was not found")
  f <- withr::local_tempfile(fileext=".json")
  writeLines('{"version": "1.1.0", "data": {"uuid": "x"}}', f)
  expect_error(readAOEF(f), "is not an AOEF file")
  writeLines("[1, 2]", f)
  expect_error(readAOEF(f), "is not an AOEF file")
  writeLines("3", f)
  expect_error(readAOEF(f), "is not an AOEF file")
})

test_that("readAOEF names what a file refers to and does not hold", {
  expect_error(readAOEF(aoefDocument(aoefMinimal(clips=list()))), "refers to clip")
  expect_error(readAOEF(aoefDocument(aoefMinimal(recordings=list()))), "refers to recording")
  expect_error(readAOEF(aoefDocument(aoefMinimal(sound_events=list()))), "refers to sound event")
  expect_error(readAOEF(aoefDocument(aoefMinimal(sound_event_annotations=list()))),
               "refers to sound event annotation")
  expect_error(readAOEF(aoefDocument(aoefMinimal(clip_annotations=list(list(uuid="x"))))), "refers to clip")
})

test_that("readAOEF refuses a recording or a clip without what it must have", {
  pathless <- aoefMinimal(recordings=list(list(uuid="00000000-0000-4000-8000-000000000001", duration=10,
                                               channels=1, samplerate=48000)))
  expect_error(readAOEF(aoefDocument(pathless)), "has no path")
  timeless <- aoefMinimal(clips=list(list(uuid="00000000-0000-4000-8000-000000000002",
                                          recording="00000000-0000-4000-8000-000000000001", start_time=0)))
  expect_error(readAOEF(aoefDocument(timeless)), "has no start or end time")
  #An annotation may not end before it starts.
  backwards <- aoefMinimal(clips=list(list(uuid="00000000-0000-4000-8000-000000000002",
                                           recording="00000000-0000-4000-8000-000000000001",
                                           start_time=10, end_time=0)))
  expect_error(readAOEF(aoefDocument(backwards)), "ends before it starts")
  #A tag without a value, which soundevent does not write, is read with an
  #empty one.
  valueless <- aoefMinimal(tags=list(list(id=0, key="species")))
  a <- readAOEF(aoefDocument(valueless))
  expect_equal(c(a[[1]]@type, a[[1]]@value), c("species", ""))
})

test_that("readAOEF refuses a shape it cannot read", {
  shaped <- function(geometry) {
    aoefMinimal(sound_events=list(list(uuid="00000000-0000-4000-8000-000000000003",
                                       recording="00000000-0000-4000-8000-000000000001",
                                       geometry=geometry)))
  }
  expect_error(readAOEF(aoefDocument(shaped(NULL))), "has no shape")
  expect_error(readAOEF(aoefDocument(shaped(list(type="Circle", coordinates=list(1, 2))))),
               "unknown type Circle")
  expect_error(readAOEF(aoefDocument(shaped(list(type="BoundingBox", coordinates=list(1, 2))))),
               "BoundingBox shape of 2 coordinates")
  expect_error(readAOEF(aoefDocument(shaped(list(type="Polygon", coordinates=list(list(list(1, 2, 3))))))),
               "Polygon shape of 3 coordinates")
  #soundevent swaps the ends of an interval written backwards, and so does this.
  a <- readAOEF(aoefDocument(shaped(list(type="TimeInterval", coordinates=list(2, 1)))))
  expect_equal(c(a[[1]]@start, a[[1]]@end), c(1, 2))
})

test_that("Annotations read from AOEF are written back as they were", {
  original <- readAOEF(aoefFixture("soundevent_annotation_project"), audio.dir="D:/audio")
  f <- withr::local_tempfile(fileext=".json")
  expect_equal(writeAOEF(original, f, audio.dir="D:/audio", collection="annotation_project"), f)
  expect_equal(readAOEF(f, audio.dir="D:/audio"), original)

  written <- aoefJSON(f)$data
  expect_equal(written$collection_type, "annotation_project")
  expect_length(written$tasks, 3)
  expect_equal(vapply(written$recordings, function(r) r$path, character(1)), c("site1/long.wav", "site2/short.wav"))
  shapes <- vapply(written$sound_events, function(s) s$geometry$type, character(1))
  expect_true(all(c("Polygon", "MultiPolygon", "LineString", "MultiLineString", "MultiPoint") %in% shapes))

  for (name in c("soundevent_example_evaluation_set", "soundevent_example_model_run", "soundevent_nips4b_plus_sample")) {
    a <- readAOEF(aoefFixture(name))
    writeAOEF(a, f)
    b <- readAOEF(f)
    #Predictions are written as annotations, so their scores are not kept.
    strip <- function(x) {
      x@source <- ""
      x@metadata$score <- NULL
      x@metadata$tag_score <- NULL
      x@metadata$aoef$object <- sub("prediction", "annotation", x@metadata$aoef$object)
      x
    }
    expect_equal(lapply(b, strip), lapply(a, strip))
  }
})

test_that("Lists are written as the JSON jsonlite reads back", {
  x <- list(a=0.1, b=0.13796845654895182, c=2L, d="quote \" slash \\ line\nbell\001", e=c(1.5, 2),
            f=list(), g=TRUE, h=FALSE, i=list(list(1, 2e-20), "z"), j=NA, k=c(p=1, q=2), l=1e21)
  json <- sonicscrewdriver:::.aoefJSON(x)
  #The fewest digits that give a number back exactly, as Python writes them.
  expect_true(grepl('"a":0.1,', json, fixed=TRUE))
  expect_true(grepl('"b":0.13796845654895182,', json, fixed=TRUE))
  expect_true(grepl('"d":"quote \\" slash \\\\ line\\u000abell\\u0001"', json, fixed=TRUE))
  back <- jsonlite::parse_json(json)
  expect_identical(back$b, 0.13796845654895182)
  expect_identical(back$d, x$d)
  expect_equal(back$e, list(1.5, 2L))
  #A field holding nothing is left out, as AOEF leaves it out.
  expect_false("f" %in% names(back))
  expect_equal(sonicscrewdriver:::.aoefJSON(list()), "[]")
  expect_equal(sonicscrewdriver:::.aoefJSON(list(list(), list(1))), "[[],[1]]")
  #A null read in an array is written back as one.
  expect_equal(sonicscrewdriver:::.aoefJSON(jsonlite::parse_json("[null,1,[null]]")), "[null,1,[null]]")
  expect_identical(c(back$g, back$h), c(TRUE, FALSE))
  expect_equal(back$i, list(list(1L, 2e-20), "z"))
  expect_null(back$j)
  expect_equal(back$k, list(1L, 2L))
  expect_equal(back$l, 1e21)
  expect_error(sonicscrewdriver:::.aoefJSON(list(a=Inf)), "value of Inf cannot be written")
})

test_that("writeAOEF writes annotations made here, with the shapes they describe", {
  f <- normalizePath(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"), "/")
  annotations <- list(
    annotation(file=f, start=0, end=3, type="birdnet-detection", value="Turdus merula",
               identifiedBy="BirdNET", dateIdentified=as.POSIXct("2026-10-30 09:15:00", tz="UTC"),
               metadata=list(confidence=0.9, notes=c("one", "two"), features=c(snr=4, bad=NA))),
    annotation(file=f, start=1, end=Inf, low=1000, value="label only"),
    annotation(file=f, start=2, end=2),
    annotation(file=f, start=2.5, end=2.5, low=3000, high=3000, type="t", value="v"),
    annotation(file=f, start=3, end=4, low=500, high=900, type=NA, value=NA,
               identifiedBy=NA, dateIdentified="2026-10-30T10:00:00", metadata=list(features="x")),
    #AOEF holds no date without a time of day, so this one's date is not written.
    annotation(file=f, start=4, end=4.5, low=NA, high=NA, type="t", value="v",
               dateIdentified=as.Date("2026-10-30"), metadata=list(features=c(1, 2)))
  )
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(annotations, out, audio.dir=dirname(f))
  data <- aoefJSON(out)$data
  expect_equal(data$collection_type, "annotation_set")
  expect_null(data$name)
  expect_equal(data$recordings[[1]][c("path", "duration", "channels", "samplerate")],
               list(path="AUDIOMOTH.WAV", duration=5L, channels=1L, samplerate=48000L))
  geometry <- lapply(data$sound_events, function(s) s$geometry)
  expect_equal(vapply(geometry, function(g) g$type, character(1)),
               c("TimeInterval", "BoundingBox", "TimeStamp", "Point", "BoundingBox", "TimeInterval"))
  #An end of Inf is the end of the recording, and a high of Inf half its rate.
  expect_equal(unlist(geometry[[2]]$coordinates), c(1, 1000, 5, 24000))
  expect_equal(geometry[[3]]$coordinates, 2L)

  tags <- vapply(data$tags, function(t) paste(t$key, t$value), character(1))
  expect_equal(tags, c("birdnet-detection Turdus merula", "label label only", "t v"))
  events <- data$sound_event_annotations
  expect_equal(lapply(events, function(e) unlist(e$tags)), list(0L, 1L, NULL, 2L, NULL, 2L))
  expect_equal(vapply(events[[1]]$notes, function(n) n$message, character(1)), c("one", "two"))
  expect_equal(events[[1]]$created_on, "2026-10-30T09:15:00Z")
  expect_equal(events[[5]]$created_on, "2026-10-30T10:00:00")
  expect_null(events[[6]]$created_on)
  expect_equal(data$users[[1]]$name, "BirdNET")
  expect_equal(events[[1]]$created_by, data$users[[1]]$uuid)
  expect_null(events[[5]]$created_by)
  expect_equal(data$sound_events[[1]]$features, list(snr=4L))
  expect_null(data$sound_events[[5]]$features)
  expect_null(data$sound_events[[6]]$features)

  #soundevent reads only annotations in a clip, so they are put in one
  #spanning the recording.
  expect_length(data$clips, 1)
  expect_equal(c(data$clips[[1]]$start_time, data$clips[[1]]$end_time), c(0L, 5L))
  expect_length(data$clip_annotations[[1]]$sound_events, 6)

  back <- readAOEF(out, audio.dir=dirname(f))
  expect_equal(back[[1]]@file, f)
  expect_equal(back[[1]]@identifiedBy, "BirdNET")
  expect_equal(back[[1]]@dateIdentified, "2026-10-30T09:15:00Z")
  expect_null(back[[1]]@metadata$confidence)
})

test_that("writeAOEF reads the recording of a file that is not a wave file", {
  skip_if_not_installed("av")
  f <- normalizePath(system.file("extdata", "STEREO.mp3", package="sonicscrewdriver"), "/")
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(annotation(file=f, start=0, end=1, value="x"), out)
  recording <- aoefJSON(out)$data$recordings[[1]]
  expect_equal(recording$path, f)
  expect_equal(recording$channels, 2L)
  expect_equal(recording$samplerate, 48000L)
  expect_gt(recording$duration, 5)

  local_mocked_bindings(av_media_info=function(...) list(duration=1, audio=NULL), .package="av")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), out), "holds no audio")
})

test_that("writeAOEF needs the av package to read a file that is not a wave file", {
  f <- system.file("extdata", "STEREO.mp3", package="sonicscrewdriver")
  local_mocked_bindings(requireNamespace=function(...) FALSE, .package="base")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), tempfile()), "av package is needed")
})

test_that("writeAOEF writes an annotation project Whombat can import", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(annotation(file=f, start=0, end=1, type="species", value="x"), out,
            collection="annotation_project", description="Made here")
  data <- aoefJSON(out)$data
  expect_equal(data$collection_type, "annotation_project")
  expect_equal(data$name, tools::file_path_sans_ext(basename(out)))
  expect_equal(data$description, "Made here")
  expect_equal(data$project_tags, list(0L))
  expect_equal(data$tasks[[1]]$clip, data$clips[[1]]$uuid)

  writeAOEF(list(), out, collection="annotation_project", name="Empty")
  expect_equal(aoefJSON(out)$data$name, "Empty")
  expect_equal(readAOEF(out), list())
  expect_equal(nrow(readAOEF(out, output="data.frame")), 0)
  #NULL, which audioblast() gives when nothing matches, is no annotations.
  writeAOEF(NULL, out)
  expect_equal(readAOEF(out), list())
})

test_that("writeAOEF writes every character as ASCII, so any reader decodes it", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  value <- "Pip\u00e9strelle \u4e2d \U0001F426"
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(annotation(file=f, start=0, end=1, type="\u00e9", value=value), out)
  expect_true(all(readBin(out, "raw", file.info(out)$size) < as.raw(128)))
  expect_true(grepl("\\ud83d\\udc26", readLines(out), fixed=TRUE))
  back <- readAOEF(out)
  expect_equal(back[[1]]@value, value)
  expect_equal(back[[1]]@type, "\u00e9")
})

test_that("writeAOEF writes tags and names beginning with a dot", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(annotation(file=f, start=0, end=1, type=".key", value=".value",
                       identifiedBy=".someone"), out)
  data <- aoefJSON(out)$data
  expect_equal(data$tags[[1]][c("key", "value")], list(key=".key", value=".value"))
  expect_equal(data$users[[1]]$name, ".someone")
})

test_that("writeAOEF gives annotations changed since they were read identifiers of their own", {
  a <- readAOEF(aoefFixture("soundevent_annotation_project"), audio.dir="D:/audio")
  #The two tags of one sound event, one of them moved.
  a[[3]]@start <- 1.5
  #A polygon given a new range is written as the box it now is.
  a[[8]]@end <- 12.5
  #A sound event moved out of its clip is put in one spanning the recording.
  a[[4]]@start <- 15
  a[[4]]@end <- 16
  #The tags of a clip given a new range are put on a clip of their own.
  a[[1]]@end <- 9
  #An identifier that is not one is replaced.
  a[[6]]@metadata$aoef$uuid <- "not an identifier"
  out <- withr::local_tempfile(fileext=".json")
  writeAOEF(a, out, audio.dir="D:/audio")
  data <- aoefJSON(out)$data

  uuids <- c(
    vapply(data$sound_events, function(s) s$uuid, character(1)),
    vapply(data$sound_event_annotations, function(s) s$uuid, character(1)),
    vapply(data$clips, function(s) s$uuid, character(1)),
    vapply(data$clip_annotations, function(s) s$uuid, character(1))
  )
  expect_false(anyDuplicated(uuids) > 0)
  #Ten sound events, one of them now two.
  expect_length(data$sound_event_annotations, 11)
  written <- vapply(data$sound_event_annotations, function(s) s$uuid, character(1))
  expect_equal(written[1], a[[2]]@metadata$aoef$uuid)
  #soundevent made the identifiers of the fixture from names, as version 5.
  expect_true(all(grepl("^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$", written[c(2, 5)])))
  expect_false(any(grepl("^[0-9a-f]{8}-[0-9a-f]{4}-4", written[-c(2, 5)])))
  shapes <- vapply(data$sound_events, function(s) s$geometry$type, character(1))
  expect_false("Polygon" %in% shapes)
  spans <- t(vapply(data$clips, function(c) c(c$start_time, c$end_time), numeric(2)))
  expect_true(any(spans[, 1] == 0 & spans[, 2] == 20))
  expect_true(any(spans[, 1] == 0 & spans[, 2] == 9))

  back <- readAOEF(out, audio.dir="D:/audio")
  moved <- Filter(function(x) x@start == 15 && x@value == "Troglodytes troglodytes", back)
  expect_length(moved, 1)
  expect_equal(c(moved[[1]]@metadata$aoef$clip$start, moved[[1]]@metadata$aoef$clip$end), c(0, 20))
})

test_that("writeAOEF puts a tag on a clip from what an annotation says of it", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  out <- withr::local_tempfile(fileext=".json")
  #Features that are not finite are not written, and with none left there are
  #no features.
  clipTag <- annotation(file=f, start=1, end=Inf, type="quality", value="noisy",
                        metadata=list(features=c(snr=Inf), aoef=list(object="clip_annotation")))
  bad <- annotation(file=f, start=NA, end=1, metadata=list(aoef=list(object="clip_annotation")))
  writeAOEF(clipTag, out)
  data <- aoefJSON(out)$data
  expect_equal(c(data$clips[[1]]$start_time, data$clips[[1]]$end_time), c(1L, 5L))
  expect_null(data$clips[[1]]$features)
  expect_equal(data$clip_annotations[[1]]$tags, list(0L))
  expect_null(data$clip_annotations[[1]]$sound_events)
  expect_error(writeAOEF(bad, out), "has no start or end time")
})

test_that("writeAOEF writes paths relative to audio.dir, however it is given", {
  dir <- withr::local_tempdir()
  file.copy(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"), file.path(dir, "a.wav"))
  out <- withr::local_tempfile(fileext=".json")
  a <- annotation(file=file.path(dir, "a.wav"), start=0, end=1)
  #The directory as the system resolves it, rather than as written.
  withr::with_dir(dir, writeAOEF(a, out, audio.dir="."))
  expect_equal(aoefJSON(out)$data$recordings[[1]]$path, "a.wav")
  writeAOEF(a, out, audio.dir=gsub("/", "\\", dir, fixed=TRUE))
  expect_equal(aoefJSON(out)$data$recordings[[1]]$path, "a.wav")
  expect_error(writeAOEF(a, out, audio.dir=file.path(dir, "elsewhere")), "is not in audio.dir")
})

test_that("writeAOEF refuses what it cannot write", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  out <- tempfile(fileext=".json")
  expect_error(writeAOEF(list(1), out), "All items in list must be Annotation objects.")
  expect_error(writeAOEF("x", out), "All items in list must be Annotation objects.")
  expect_error(writeAOEF(annotation(start=0, end=1), out), "must name the file")
  expect_error(writeAOEF(annotation(file=NA, start=0, end=1), out), "must name the file")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), out, collection="dataset"), "Unknown collection.")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), out, name=c("a", "b")), "single string")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), out, description=1), "single string")
  expect_error(writeAOEF(annotation(file=f, start=0, end=1), out, name=NA_character_), "single string")
  expect_error(writeAOEF(annotation(file=f, start=NA, end=1), out), "has no start or end time")
  expect_error(writeAOEF(annotation(file=tempfile(fileext=".wav"), start=0, end=1), out),
               "was not found")
  expect_false(file.exists(out))
})

test_that("A shape kept that cannot be read is written from the range of its annotation", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  out <- withr::local_tempfile(fileext=".json")
  a <- annotation(file=f, start=0, end=1, metadata=list(aoef=list(geometry=list(type="Nonsense"))))
  writeAOEF(a, out)
  expect_equal(aoefJSON(out)$data$sound_events[[1]]$geometry$type, "TimeInterval")
})

test_that("Identifiers made here differ from each other, whatever the seed", {
  draws <- function() {
    set.seed(1)
    ids <- c(sonicscrewdriver:::.aoefUUID(), sonicscrewdriver:::.aoefUUID(3))
    list(ids=ids, next_draw=runif(1))
  }
  first <- draws()
  second <- draws()
  pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  expect_true(all(grepl(pattern, c(first$ids, second$ids))))
  expect_length(unique(c(first$ids, second$ids)), 8)
  #Making them draws nothing from R's random numbers.
  set.seed(1)
  expect_equal(first$next_draw, runif(1))

  #A process forked from this one begins its identifiers afresh.
  made <- sonicscrewdriver:::.aoefMade
  pid <- made$pid
  withr::defer(assign("pid", pid, envir=made))
  assign("pid", -1L, envir=made)
  forked <- sonicscrewdriver:::.aoefUUID(2)
  expect_true(all(grepl(pattern, forked)))
  expect_false(any(forked %in% c(first$ids, second$ids)))
  expect_false(identical(substr(forked[1], 1, 20), substr(first$ids[1], 1, 20)))
})
