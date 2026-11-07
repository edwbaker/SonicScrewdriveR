#The version of AOEF read and written here. soundevent refuses to load a file
#written in any other.
.aoefVersion <- "1.1.0"

#The kinds of AOEF object an annotation is read from, which writeAOEF() reads
#back to know a tag on a whole clip from a sound event.
.aoefClipObjects <- c("clip_annotation", "clip_prediction")

#' Read an AOEF file
#'
#' Reads the annotations held in a file in the Acoustic Objects Exchange Format
#' (AOEF), the JSON format of the soundevent Python package, in which the
#' Whombat annotation tool \insertCite{whombat2023}{sonicscrewdriver} exports
#' its projects. The predictions of a model run are read in the same way.
#'
#' AOEF gives each sound event a shape: a stretch of time, a box of time and
#' frequency, a single moment, a single point, or lines and polygons drawn on
#' the spectrogram. Each is read as the smallest range of time and frequency
#' that holds it, which for a stretch of time or a single moment is every
#' frequency.
#'
#' A sound event can carry several tags, each a key and a value such as
#' `dwc:scientificName` and `Turdus merula`, while an `Annotation` has one
#' `type` and one `value`. So a sound event is read as one annotation for each
#' of its tags, with the key as its `type`, or as one annotation with no type
#' and no value where it has no tags. The tags of a clip, a part of a recording
#' annotated as a whole, are read in the same way as annotations spanning the
#' clip. A clip with no tags and no sound events, annotated as holding nothing,
#' is read as one annotation with no type and no value, so that the absence is
#' kept.
#'
#' Who made each annotation (`created_by`) is its `identifiedBy`, by the
#' person's name, and when (`created_on`) its `dateIdentified`. The `metadata`
#' of each annotation holds its `notes`, the `features` measured of it, and for a
#' prediction its `score` and the `tag_score` of its tag. Under `aoef` it also
#' holds the identifiers and the exact shape that [writeAOEF()] needs to write
#' it back as it was, and what the file said of its recording. Sequences of
#' sound events are not read.
#'
#' soundevent on Windows writes in the encoding of the locale rather than in
#' UTF-8, so a file that is not UTF-8 is read as Windows-1252.
#'
#' @param file Path to the AOEF file.
#' @param audio.dir The directory the paths of the recordings in the file are
#'   relative to, as soundevent's `audio_dir` is. Where it is `NULL` the paths
#'   are given as the file has them.
#' @param output One of "Annotation" or "data.frame".
#' @references
#'   \insertAllCited{}
#' @return A list of Annotation objects, or a data frame if output is
#'   "data.frame".
#' @seealso [writeAOEF()]
#' @export
readAOEF <- function(file, audio.dir=NULL, output="Annotation") {
  .validateChoice(output, c("Annotation", "data.frame"), msg="Unknown output format.")
  doc <- .aoefParse(file)
  #Not doc$data, which matches a name beginning "data" where there is none.
  data <- if (is.list(doc) && "data" %in% names(doc)) doc[["data"]]
  if (!is.list(data) || is.null(data$collection_type)) {
    stop("'", file, "' is not an AOEF file: it holds no collection.", call.=FALSE)
  }
  if (!identical(doc[["version"]], .aoefVersion)) {
    said <- if (is.null(doc[["version"]])) "no version" else paste("version", doc[["version"]])
    warning("'", file, "' is written in ", said, " of AOEF, ",
            "and this reads version ", .aoefVersion, ".", call.=FALSE)
  }

  recordings <- .aoefIndex(data$recordings, "uuid")
  context <- list(
    tags=.aoefIndex(data$tags, "id"),
    users=.aoefIndex(data$users, "uuid"),
    recordings=recordings,
    #What is kept of each recording is worked out once, not once per annotation.
    kept=list2env(eapply(recordings, .aoefKeptRecording, all.names=TRUE), parent=emptyenv()),
    clips=.aoefIndex(data$clips, "uuid"),
    sound_events=.aoefIndex(data$sound_events, "uuid"),
    audio.dir=audio.dir,
    source=.aoefSource(data),
    blank=new("Annotation")
  )
  ret <- c(
    .aoefReadObjects(data$clip_annotations, data$sound_event_annotations, context, prediction=FALSE),
    .aoefReadObjects(data$clip_predictions, data$sound_event_predictions, context, prediction=TRUE)
  )
  if (output == "data.frame") {
    return(AnnotationList2DataFrame(ret))
  }
  return(ret)
}

#' Parse an AOEF file
#'
#' @param file Path to the file.
#' @return The parsed JSON, as lists.
#' @keywords internal
#' @noRd
.aoefParse <- function(file) {
  if (!file.exists(file)) {
    stop("'", file, "' was not found.", call.=FALSE)
  }
  bytes <- readBin(file, "raw", n=file.info(file)$size)
  #A byte order mark, which a JSON parser takes for the start of a value.
  if (length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    bytes <- bytes[-(1:3)]
  }
  text <- rawToChar(bytes)
  if (validUTF8(text)) {
    Encoding(text) <- "UTF-8"
  } else {
    text <- iconv(text, from="CP1252", to="UTF-8")
  }
  return(jsonlite::parse_json(text))
}

#' The source of the annotations of a file
#'
#' The predictions of a model run come from the model it names. A model run
#' written before soundevent gave the model a field of its own names it as the
#' run, which soundevent still reads.
#' @param data The collection of the file.
#' @return The name of the model, or "readAOEF".
#' @keywords internal
#' @noRd
.aoefSource <- function(data) {
  if (identical(data$collection_type, "model_run")) {
    for (name in list(data$model[["name"]], data[["name"]])) {
      if (is.character(name) && length(name) == 1 && nzchar(name)) {
        return(name)
      }
    }
  }
  return("readAOEF")
}

#' The identifiers of a list of AOEF objects
#'
#' @param objects A list of objects, or NULL.
#' @param key The field that identifies them.
#' @return A character vector, with "" for an object without one.
#' @keywords internal
#' @noRd
.aoefIds <- function(objects, key) {
  return(vapply(objects, function(x) paste(as.character(x[[key]]), collapse=""), character(1)))
}

#' Index a list of AOEF objects by their identifiers
#'
#' An environment rather than a named list, as finding a name in a list looks
#' at every name before it, which in a project of thousands of sound events
#' took most of the time it was read in.
#' @param objects A list of objects, or NULL.
#' @param key The field that identifies them.
#' @return An environment holding each object under its identifier, the first
#'   where two share one.
#' @keywords internal
#' @noRd
.aoefIndex <- function(objects, key) {
  ids <- .aoefIds(objects, key)
  keep <- nzchar(ids) & !duplicated(ids)
  objects <- as.list(objects)[keep]
  names(objects) <- ids[keep]
  return(list2env(objects, parent=emptyenv()))
}

#' Find an object a file refers to
#'
#' @param index An environment made by .aoefIndex().
#' @param id The identifier referred to.
#' @param what What kind of object it is, for the error.
#' @return The object.
#' @keywords internal
#' @noRd
.aoefLookup <- function(index, id, what) {
  id <- if (length(id) == 1) as.character(id) else ""
  found <- if (!is.na(id) && nzchar(id)) index[[id]]
  if (is.null(found)) {
    stop("The file refers to ", what, " ", format(id), ", which it does not hold.", call.=FALSE)
  }
  return(found)
}

#' Drop the empty fields of a list
#'
#' AOEF leaves out a field with nothing in it, as soundevent writes no field
#' that is None.
#' @param x A list.
#' @return The list without its NULL or empty fields.
#' @keywords internal
#' @noRd
.aoefDrop <- function(x) {
  return(x[lengths(x) > 0])
}

#' What is kept of a recording in the metadata of its annotations
#'
#' Its path is not kept, being the file of the annotation.
#' @param recording A recording object.
#' @return A list.
#' @keywords internal
#' @noRd
.aoefKeptRecording <- function(recording) {
  fields <- c("uuid", "duration", "channels", "samplerate", "time_expansion", "hash",
              "date", "time", "latitude", "longitude", "rights")
  return(.aoefDrop(recording[fields[fields %in% names(recording)]]))
}

#' The path of a recording
#'
#' Paths written by soundevent on Windows separate their parts with
#' backslashes, which elsewhere are part of a name. A path that is absolute
#' already is not put under the directory, as Python does not.
#' @param path The path the file gives.
#' @param dir The directory it is relative to, or NULL.
#' @return A path.
#' @keywords internal
#' @noRd
.aoefPath <- function(path, dir) {
  path <- gsub("\\", "/", path, fixed=TRUE)
  if (is.null(dir) || grepl("^(/|[A-Za-z]:)", path)) {
    return(path)
  }
  return(file.path(sub("[/\\\\]+$", "", dir), path))
}

#' The name of the person who made an annotation
#'
#' Whombat finds the people of a file it imports among its own by name, so the
#' name comes first.
#' @param users The users of the file, named by .aoefIndex().
#' @param id The identifier of the user, or NULL.
#' @return A name, or NULL.
#' @keywords internal
#' @noRd
.aoefUserName <- function(users, id) {
  user <- if (is.character(id) && length(id) == 1 && nzchar(id)) users[[id]]
  if (is.null(user)) {
    return(if (length(id) == 1) id else NULL)
  }
  for (field in c("name", "username", "email", "uuid")) {
    if (is.character(user[[field]]) && nzchar(user[[field]])) {
      return(user[[field]])
    }
  }
}

#' Read the annotations or predictions of a file
#'
#' Each clip is read in the order of the file, its own tags first and then the
#' sound events in it. Sound events in no clip come last: soundevent leaves
#' them out, but they are in the file.
#' @param clipObjects The clip annotations or clip predictions of the file.
#' @param eventObjects The sound event annotations or predictions of the file.
#' @param context What .aoefReadObjects() needs of the rest of the file.
#' @param prediction Whether these are predictions.
#' @return A list of Annotation objects.
#' @keywords internal
#' @noRd
.aoefReadObjects <- function(clipObjects, eventObjects, context, prediction) {
  events <- eventObjects
  read <- rep(FALSE, length(events))
  pieces <- list()
  kinds <- if (prediction) c("clip_prediction", "sound_event_prediction") else c("clip_annotation", "sound_event_annotation")
  #Where each sound event a clip lists is, found for every clip at once.
  members <- lapply(clipObjects, function(object) as.character(unlist(object$sound_events)))
  positions <- match(unlist(members), .aoefIds(events, "uuid"))
  last <- cumsum(lengths(members))
  for (j in seq_along(clipObjects)) {
    object <- clipObjects[[j]]
    clip <- .aoefLookup(context$clips, object$clip, "clip")
    if (!is.numeric(clip$start_time) || !is.numeric(clip$end_time)) {
      stop("Clip ", object$clip, " in the file has no start or end time.", call.=FALSE)
    }
    #An annotation may not end before it starts, and the annotations of a clip
    #span it.
    if (isTRUE(clip$start_time > clip$end_time)) {
      stop("Clip ", object$clip, " in the file ends before it starts.", call.=FALSE)
    }
    held <- .aoefDrop(list(uuid=clip$uuid, annotation=object$uuid, start=clip$start_time,
                           end=clip$end_time, created_on=object$created_on))
    if (length(object$tags) > 0 || length(object$notes) > 0 || length(members[[j]]) == 0) {
      #The features of a clip are measured of it, and a clip prediction can
      #carry some of its own.
      features <- c(unlist(clip$features), unlist(object$features))
      pieces[[length(pieces) + 1]] <- .aoefAnnotations(
        object, kinds[1], c(clip$start_time, clip$end_time, 0, Inf), clip$recording,
        held, NULL, features, context, prediction
      )
    }
    for (k in seq_along(members[[j]])) {
      i <- positions[last[j] - length(members[[j]]) + k]
      if (is.na(i)) {
        stop("The file refers to ", gsub("_", " ", kinds[2]), " ", members[[j]][k],
             ", which it does not hold.", call.=FALSE)
      }
      read[i] <- TRUE
      pieces[[length(pieces) + 1]] <- .aoefEventAnnotations(events[[i]], kinds[2], held, context, prediction)
    }
  }
  for (i in which(!read)) {
    pieces[[length(pieces) + 1]] <- .aoefEventAnnotations(events[[i]], kinds[2], NULL, context, prediction)
  }
  return(do.call(c, c(list(list()), pieces)))
}

#' The annotations of one sound event annotation or prediction
#'
#' @param object The sound event annotation or prediction.
#' @param kind What kind of object it is.
#' @param clip What is kept of the clip it is in, or NULL.
#' @param context What .aoefReadObjects() needs of the rest of the file.
#' @param prediction Whether it is a prediction.
#' @return A list of Annotation objects.
#' @keywords internal
#' @noRd
.aoefEventAnnotations <- function(object, kind, clip, context, prediction) {
  event <- .aoefLookup(context$sound_events, object$sound_event, "sound event")
  return(.aoefAnnotations(
    object, kind, .aoefBounds(event$geometry), event$recording, clip, event,
    unlist(event$features), context, prediction
  ))
}

#' The annotations of one object of a file, one for each of its tags
#'
#' @param object The clip or sound event annotation or prediction.
#' @param kind What kind of object it is.
#' @param bounds Its start, end, low and high.
#' @param recording The identifier of its recording.
#' @param clip What is kept of the clip it is in, or NULL.
#' @param event The sound event it annotates, or NULL for a clip.
#' @param features The features measured of it, as a named vector, or NULL.
#' @param context What .aoefReadObjects() needs of the rest of the file.
#' @param prediction Whether it is a prediction.
#' @return A list of Annotation objects.
#' @keywords internal
#' @noRd
.aoefAnnotations <- function(object, kind, bounds, recording, clip, event, features, context, prediction) {
  path <- .aoefLookup(context$recordings, recording, "recording")$path
  if (!is.character(path) || length(path) != 1) {
    stop("Recording ", recording, " in the file has no path.", call.=FALSE)
  }
  identifiedBy <- .aoefUserName(context$users, object$created_by)
  dateIdentified <- object$created_on
  metadata <- .aoefDrop(list(
    notes=if (length(object$notes) > 0) vapply(object$notes, function(n) n$message, character(1)),
    features=if (length(features) > 0) features,
    score=if (prediction) object$score
  ))
  aoef <- .aoefDrop(list(
    object=kind,
    uuid=object$uuid,
    sound_event=event$uuid,
    geometry=event$geometry,
    clip=clip,
    user=object$created_by,
    recording=context$kept[[recording]]
  ))

  #A prediction gives each tag with its score, as [id, score].
  tags <- object$tags
  ids <- vapply(tags, function(t) as.character(t[[1]]), character(1))
  scores <- if (prediction) vapply(tags, function(t) as.numeric(t[[2]]), numeric(1)) else rep(NA_real_, length(tags))
  #soundevent passes over a tag the file does not hold, and so does this.
  known <- vapply(ids, function(id) !is.null(context$tags[[id]]), logical(1), USE.NAMES=FALSE)
  ids <- ids[known]
  scores <- scores[known]
  if (length(ids) == 0) {
    ids <- NA_character_
    scores <- NA_real_
  }

  #The slots of one annotation are filled in rather than an annotation being
  #made with annotation() for each tag: making one checks every slot of the
  #new object, which took most of the time a large project was read in. The
  #values are coerced as annotation() coerces them, and they keep the rules
  #the class checks: each slot holds one value, the start and low are the
  #least of a range and the end and high the greatest, and a clip that ends
  #before it starts is refused above.
  base <- context$blank
  base@file <- as.character(.aoefPath(path, context$audio.dir))
  base@start <- as.numeric(bounds[1])
  base@end <- as.numeric(bounds[2])
  base@low <- as.numeric(bounds[3])
  base@high <- as.numeric(bounds[4])
  base@source <- as.character(context$source)
  base@identifiedBy <- if (is.character(identifiedBy) && length(identifiedBy) == 1) identifiedBy else ""
  base@dateIdentified <- if (is.character(dateIdentified) && length(dateIdentified) == 1) dateIdentified else ""
  ret <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    tag <- if (is.na(ids[i])) list(key="", value="") else context$tags[[ids[i]]]
    m <- metadata
    if (!is.na(scores[i])) {
      m$tag_score <- scores[i]
    }
    m$aoef <- aoef
    ret[[i]] <- base
    ret[[i]]@metadata <- m
    #A tag without a key or a value, which soundevent does not write, has an
    #empty one rather than none.
    ret[[i]]@type <- if (length(tag$key) == 1) as.character(tag$key) else ""
    ret[[i]]@value <- if (length(tag$value) == 1) as.character(tag$value) else ""
  }
  return(ret)
}

#' The smallest range of time and frequency holding a shape
#'
#' @param geometry An AOEF geometry, as parsed.
#' @return The start, end, low and high of the range.
#' @keywords internal
#' @noRd
.aoefBounds <- function(geometry) {
  type <- geometry$type
  coordinates <- as.numeric(unlist(geometry$coordinates))
  sizes <- list(TimeStamp=1, TimeInterval=2, BoundingBox=4, Point=2)
  if (!is.character(type) || length(type) != 1) {
    stop("A sound event in the file has no shape.", call.=FALSE)
  }
  expected <- sizes[[type]]
  shaped <- type %in% c("LineString", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon")
  if ((is.null(expected) && !shaped)) {
    stop("A sound event in the file has a shape of unknown type ", type, ".", call.=FALSE)
  }
  if ((!is.null(expected) && length(coordinates) != expected) ||
      (shaped && (length(coordinates) == 0 || length(coordinates) %% 2 != 0))) {
    stop("A sound event in the file has a ", type, " shape of ", length(coordinates),
         " coordinates.", call.=FALSE)
  }
  if (type == "TimeStamp") {
    return(c(coordinates, coordinates, 0, Inf))
  }
  if (type == "TimeInterval") {
    return(c(range(coordinates), 0, Inf))
  }
  #A box is written as start, low, end, high, and every other shape as points
  #of time and frequency, so times and frequencies alternate in all of them.
  time <- coordinates[c(TRUE, FALSE)]
  frequency <- coordinates[c(FALSE, TRUE)]
  return(c(range(time), range(frequency)))
}

#' Write an AOEF file
#'
#' Writes a list of `Annotation` objects to a file in the Acoustic Objects
#' Exchange Format (AOEF), as an annotation set or as an annotation project,
#' which the soundevent Python package and the Whombat annotation tool
#' \insertCite{whombat2023}{sonicscrewdriver} read.
#'
#' Each annotation is written as a sound event, tagged with its `type` as the
#' key and its `value` as the value. An annotation with a value and no type is
#' tagged with the key "label", and one with neither is written without a tag.
#' Its shape is a stretch of time where it has no frequency limits, a single
#' moment where it also starts where it ends, and otherwise a box of time and
#' frequency, or a single point where the box has no size. An end of `Inf` is
#' the end of the recording, and a high frequency of `Inf` above a low one is
#' half the sample rate.
#'
#' Annotations read by [readAOEF()] are written back as they were read: those
#' of one sound event with several tags as one sound event again, a line or
#' polygon as that shape where its range has not been changed, the tags of a
#' clip on that clip, and each sound event in the clip it was in, under the
#' identifiers it had. A sound event moved off its clip altogether is put in a
#' clip spanning its recording, and one of several tags given a range of its
#' own becomes a sound event of its own. Whombat imports the annotations of a
#' project only for recordings it already holds under the same identifiers,
#' which those annotations keep. The `identifiedBy` of an annotation is
#' written as who made it (`created_by`) and its `dateIdentified` as when
#' (`created_on`), where that is a date and a time of day: AOEF holds no date
#' without a time, and one is not made up. `notes` and `features` in the
#' `metadata` of an annotation are written; nothing else of it is, so
#' predictions are written as annotations without their scores.
#'
#' soundevent reads only annotations in a clip, so every other annotation is
#' put in a clip spanning the whole of its recording. AOEF also needs the
#' duration, channels and sample rate of each recording, which are read from
#' the file of the annotation, or taken from what [readAOEF()] kept of it.
#' Reading a file other than a wave file needs the av package.
#'
#' Characters outside ASCII are written as escapes, so that the file reads the
#' same whatever encoding a reader takes it to be in.
#'
#' @param annotations A list of `Annotation` objects, or a single one.
#' @param file Path to the AOEF file to write.
#' @param audio.dir The directory the paths of the recordings are to be
#'   written relative to, which every annotated file must be in. Where it is
#'   `NULL` the paths are written as the annotations have them.
#' @param collection One of "annotation_set" or "annotation_project". Whombat
#'   imports only annotation projects, which are annotation sets with a task
#'   for each clip, and whose tags, the ones offered to those annotating it,
#'   are every tag written.
#' @param name,description A name and a description of the collection. An
#'   annotation project is named after the file where no name is given.
#' @references
#'   \insertAllCited{}
#' @return Invisibly, the path of the file written.
#' @seealso [readAOEF()]
#' @importFrom tools file_path_sans_ext
#' @export
writeAOEF <- function(annotations, file, audio.dir=NULL, collection="annotation_set",
                      name=NULL, description=NULL) {
  annotations <- .annotationList(annotations)
  .validateChoice(collection, c("annotation_set", "annotation_project"), msg="Unknown collection.")
  for (given in list(name, description)) {
    if (!is.null(given) && !(is.character(given) && length(given) == 1 && !is.na(given))) {
      stop("A name or description must be a single string.")
    }
  }
  if (collection == "annotation_project" && is.null(name)) {
    name <- file_path_sans_ext(basename(file))
  }
  annotations <- unname(annotations)
  issued <- new.env()

  #Each recording once, in the order the annotations name them.
  files <- vapply(annotations, function(a) a@file, character(1))
  if (any(is.na(files) | !nzchar(files))) {
    stop("Every annotation must name the file it annotates.")
  }
  aoefs <- lapply(annotations, .aoefOf)
  #What readAOEF() kept of a recording is taken from the first annotation of
  #it that has it, and otherwise the recording is read.
  complete <- vapply(aoefs, function(x) {
    is.list(x$recording) && all(c("duration", "channels", "samplerate") %in% names(x$recording))
  }, logical(1))
  keptAt <- which(complete)[match(unique(files), files[complete])]
  recordings <- list()
  for (j in seq_along(keptAt)) {
    f <- unique(files)[j]
    kept <- if (is.na(keptAt[j])) .aoefAudio(f) else aoefs[[keptAt[j]]]$recording
    recordings[[f]] <- c(
      list(
        uuid=.aoefClaim(issued, kept$uuid),
        path=.aoefRelativePath(f, audio.dir),
        duration=as.numeric(kept$duration),
        channels=as.integer(kept$channels),
        samplerate=as.integer(kept$samplerate)
      ),
      kept[intersect(c("time_expansion", "hash", "date", "time", "latitude", "longitude", "rights"), names(kept))]
    )
  }

  tags <- new.env()
  users <- new.env()
  #The clips, in an environment as a project can have thousands, and the order
  #they were made in.
  clips <- new.env()
  clipOrder <- character(0)
  #A clip a sound event is put in, made the first time it is asked for.
  clipFor <- function(key, recording, start, end, kept) {
    if (is.null(clips[[key]])) {
      clipOrder <<- c(clipOrder, key)
      clips[[key]] <- list(
        uuid=.aoefClaim(issued, kept$uuid),
        annotation=.aoefClaim(issued, kept$annotation),
        recording=recording$uuid,
        start=start,
        end=end,
        tags=integer(0),
        events=character(0),
        created_on=.aoefTime(kept$created_on)
      )
    }
    return(key)
  }

  #The annotations of one sound event: read by readAOEF() they share its
  #identifier and its range, and otherwise each annotation is a sound event of
  #its own.
  clipLevel <- vapply(aoefs, function(x) isTRUE(x$object %in% .aoefClipObjects), logical(1))
  ids <- vapply(aoefs, function(x) {
    if (is.character(x$uuid) && length(x$uuid) == 1 && !is.na(x$uuid)) x$uuid else NA_character_
  }, character(1))
  ids[is.na(ids)] <- paste0("\001", which(is.na(ids)))
  number <- function(name) sprintf("%.17g", vapply(annotations, slot, numeric(1), name))
  key <- paste(ids, files, number("start"), number("end"), number("low"), number("high"), sep="\r")

  events <- list()
  eventAnnotations <- list()
  for (group in split(seq_along(annotations), factor(key, levels=unique(key)))) {
    first <- annotations[[group[1]]]
    aoef <- aoefs[[group[1]]]
    recording <- recordings[[first@file]]
    if (clipLevel[group[1]]) {
      start <- first@start
      end <- if (isTRUE(is.infinite(first@end))) recording$duration else first@end
      if (!is.finite(start) || !is.finite(end)) {
        stop("An annotation of '", first@file, "' has no start or end time.")
      }
      held <- aoef$clip
      same <- is.list(held) && isTRUE(all.equal(c(held$start, held$end), c(start, end)))
      k <- clipFor(
        if (same) paste("clip", held$uuid, first@file) else paste("span", first@file, start, end),
        recording, start, end, if (same) held else list()
      )
      clips[[k]]$tags <- union(clips[[k]]$tags, .aoefTags(tags, annotations[group]))
      clips[[k]]$notes <- first@metadata[["notes"]]
      clips[[k]]$features <- first@metadata[["features"]]
      when <- .aoefTime(first@dateIdentified)
      if (!is.null(when)) {
        clips[[k]]$created_on <- when
      }
      next
    }

    geometry <- .aoefGeometry(first, recording, aoef$geometry)
    bounds <- .aoefBounds(geometry)
    #A sound event stays in the clip it was read in while it still overlaps it:
    #Whombat lets one run over the edge of its clip.
    held <- aoef$clip
    overlaps <- is.list(held) && is.numeric(held$start) && is.numeric(held$end) &&
      bounds[1] <= held$end && bounds[2] >= held$start
    k <- if (overlaps) {
      clipFor(paste("clip", held$uuid, first@file), recording, held$start, held$end, held)
    } else {
      clipFor(paste("whole", first@file), recording, 0, recording$duration, list())
    }
    event <- .aoefClaim(issued, aoef$sound_event)
    events[[length(events) + 1]] <- .aoefDrop(list(
      uuid=event,
      recording=recording$uuid,
      geometry=geometry,
      features=.aoefFeatures(first@metadata[["features"]])
    ))
    uuid <- .aoefClaim(issued, aoef$uuid)
    user <- first@identifiedBy
    eventAnnotations[[length(eventAnnotations) + 1]] <- .aoefDrop(list(
      uuid=uuid,
      sound_event=event,
      tags=as.list(.aoefTags(tags, annotations[group])),
      notes=.aoefNotes(issued, first@metadata[["notes"]]),
      created_by=if (is.character(user) && length(user) == 1 && !is.na(user) && nzchar(user)) {
        .aoefUser(users, issued, user, aoef$user)
      },
      created_on=.aoefTime(first@dateIdentified)
    ))
    clips[[k]]$events <- c(clips[[k]]$events, uuid)
  }

  #all.names, or a key or a name beginning with a dot would be left out.
  tagIds <- mget(ls(tags, all.names=TRUE, sorted=FALSE), envir=tags)
  tagIds <- tagIds[order(vapply(tagIds, function(id) id, integer(1)))]
  tagList <- lapply(names(tagIds), function(k) {
    parts <- strsplit(k, "\r", fixed=TRUE)[[1]]
    list(id=tagIds[[k]], key=parts[1], value=if (length(parts) > 1) parts[2] else "")
  })
  userList <- lapply(ls(users, all.names=TRUE), function(name) list(uuid=users[[name]], name=name))
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz="UTC")
  clips <- unname(mget(clipOrder, envir=clips))

  data <- .aoefDrop(list(
    uuid=.aoefClaim(issued, NULL),
    collection_type=collection,
    name=name,
    description=description,
    users=userList,
    tags=tagList,
    recordings=unname(recordings),
    sound_events=events,
    clips=lapply(clips, function(clip) .aoefDrop(list(
      uuid=clip$uuid,
      recording=clip$recording,
      start_time=clip$start,
      end_time=clip$end,
      features=.aoefFeatures(clip$features)
    ))),
    sound_event_annotations=eventAnnotations,
    clip_annotations=lapply(clips, function(clip) .aoefDrop(list(
      uuid=clip$annotation,
      clip=clip$uuid,
      tags=if (length(clip$tags) > 0) as.list(as.integer(clip$tags)),
      sound_events=if (length(clip$events) > 0) as.list(clip$events),
      notes=.aoefNotes(issued, clip$notes),
      created_on=clip$created_on
    ))),
    created_on=now
  ))
  if (collection == "annotation_project") {
    data$project_tags <- lapply(tagList, function(t) t$id)
    data$tasks <- lapply(clips, function(clip) list(uuid=.aoefClaim(issued, NULL), clip=clip$uuid))
  }
  json <- .aoefJSON(list(version=.aoefVersion, created_on=now, data=data))
  writeLines(.aoefAscii(json), file, useBytes=TRUE)
  return(invisible(file))
}

#' What readAOEF() kept of where an annotation came from
#'
#' @param annotation An Annotation object.
#' @return A list, empty where it was not read from an AOEF file.
#' @keywords internal
#' @noRd
.aoefOf <- function(annotation) {
  aoef <- annotation@metadata$aoef
  return(if (is.list(aoef)) aoef else list())
}

#' Claim an identifier for an object being written
#'
#' Each object written needs an identifier of its own. An object read by
#' readAOEF() keeps the one it had unless something already written has it,
#' as when the annotations of one sound event have been given different times.
#' @param issued An environment of the identifiers given out so far.
#' @param uuid The identifier the object had, or NULL.
#' @return An identifier.
#' @keywords internal
#' @noRd
.aoefClaim <- function(issued, uuid) {
  if (!(.aoefIsUUID(uuid) && is.null(issued[[uuid]]))) {
    uuid <- .aoefUUID()
  }
  assign(uuid, TRUE, envir=issued)
  return(uuid)
}

#' Whether text is an identifier
#'
#' Tested a character at a time rather than with a pattern, which in writing a
#' large project took a fifth of the time.
#' @param x Anything.
#' @return TRUE where x is a UUID, as 8, 4, 4, 4 and 12 hexadecimal digits
#'   parted by dashes.
#' @keywords internal
#' @noRd
.aoefIsUUID <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x, type="bytes") != 36L) {
    return(FALSE)
  }
  code <- utf8ToInt(x)
  dashes <- c(9L, 14L, 19L, 24L)
  #The codes of 0 to 9, A to F and a to f.
  return(all(code[dashes] == 45L) && all(code[-dashes] %in% c(48:57, 65:70, 97:102)))
}

#Holds what the identifiers made in this process begin with, and how many have
#been made.
.aoefMade <- new.env()

#' Make identifiers
#'
#' Each begins with a digest of the time, the machine and the process it is
#' made in, read once for each process, and ends with a count of those made in
#' that process, so that no two are the same. R's random numbers are not used:
#' a caller's set.seed() would make the same identifiers on every run, and
#' drawing from them would change what the caller draws next. A digest made
#' for each identifier, or the time read for each, took longer than all else
#' written.
#' @param n How many to make.
#' @return Identifiers in the form of version 4 UUIDs.
#' @importFrom digest digest
#' @keywords internal
#' @noRd
.aoefUUID <- function(n=1) {
  #Read again in a process forked from this one, which has the same count.
  if (!identical(.aoefMade$pid, Sys.getpid())) {
    seed <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"), Sys.info()[["nodename"]], Sys.getpid(), tempfile())
    assign("prefix", digest(seed, algo="md5", serialize=FALSE), envir=.aoefMade)
    assign("pid", Sys.getpid(), envir=.aoefMade)
    assign("count", 0, envir=.aoefMade)
  }
  count <- .aoefMade$count + seq_len(n)
  assign("count", .aoefMade$count + n, envir=.aoefMade)
  p <- .aoefMade$prefix
  #The version, 4, and the variant, 8 to b, are in their places.
  variant <- c("8", "9", "a", "b")[strtoi(substr(p, 17, 17), 16L) %% 4 + 1]
  return(sprintf("%s-%s-4%s-%s%s-%06x%06x", substr(p, 1, 8), substr(p, 9, 12), substr(p, 13, 15),
                 variant, substr(p, 18, 20), as.integer(count %/% 16777216), as.integer(count %% 16777216)))
}

#' The duration, channels and sample rate of a file
#'
#' @param file Path to an audio file.
#' @return A list.
#' @importFrom tuneR readWave
#' @keywords internal
#' @noRd
.aoefAudio <- function(file) {
  if (!file.exists(file)) {
    stop("'", file, "' was not found, so its duration, channels and sample rate ",
         "are not known.", call.=FALSE)
  }
  #Counted from the chunks rather than the header, as a file cut short holds
  #less audio than its header says.
  held <- .wavFramesHeld(file)
  if (!is.null(held)) {
    header <- readWave(file, header=TRUE)
    return(list(duration=held$frames / held$rate, channels=header$channels, samplerate=held$rate))
  }
  if (!requireNamespace("av", quietly=TRUE)) {
    stop("The av package is needed to read the duration of '", file, "'.", call.=FALSE)
  }
  info <- av::av_media_info(file)
  if (is.null(info$audio) || NROW(info$audio) == 0) {
    stop("'", file, "' holds no audio.", call.=FALSE)
  }
  return(list(duration=info$duration, channels=info$audio$channels[1], samplerate=info$audio$sample_rate[1]))
}

#' The path of a recording relative to a directory
#'
#' @param file Path to the recording.
#' @param dir The directory, or NULL.
#' @return The path, with forward slashes.
#' @keywords internal
#' @noRd
.aoefRelativePath <- function(file, dir) {
  slashes <- function(x) sub("/+$", "", gsub("\\", "/", x, fixed=TRUE))
  if (is.null(dir)) {
    return(slashes(file))
  }
  #As written first, so that the paths of files that are not there yet can be
  #written, and then as the system resolves them.
  tries <- list(
    c(slashes(file), slashes(dir)),
    c(slashes(normalizePath(file, "/", mustWork=FALSE)), slashes(normalizePath(dir, "/", mustWork=FALSE)))
  )
  for (try in tries) {
    prefix <- paste0(try[2], "/")
    if (startsWith(try[1], prefix)) {
      return(substring(try[1], nchar(prefix) + 1))
    }
  }
  stop("'", file, "' is not in audio.dir '", dir, "'.", call.=FALSE)
}

#' The shape of a sound event
#'
#' @param annotation An Annotation object.
#' @param recording The recording it is of, as written.
#' @param stored The shape readAOEF() kept, or NULL.
#' @return An AOEF geometry.
#' @keywords internal
#' @noRd
.aoefGeometry <- function(annotation, recording, stored) {
  start <- annotation@start
  end <- annotation@end
  low <- annotation@low
  high <- annotation@high
  #A shape read from a file is written back as it was while it still spans
  #what the annotation does, so that a polygon stays a polygon.
  if (is.list(stored)) {
    bounds <- tryCatch(.aoefBounds(stored), error=function(cond) NULL)
    if (!is.null(bounds) && isTRUE(all.equal(bounds, c(start, end, low, high)))) {
      return(stored)
    }
  }
  if (isTRUE(end == Inf)) {
    end <- recording$duration
  }
  if (!is.finite(start) || !is.finite(end)) {
    stop("An annotation of '", annotation@file, "' has no start or end time.", call.=FALSE)
  }
  #A low of -Inf reaches the bottom of the spectrum, as a high of Inf reaches
  #the top.
  low <- if (is.na(low) || is.infinite(low)) 0 else low
  high <- if (is.na(high)) Inf else high
  if (low <= 0 && is.infinite(high)) {
    if (start == end) {
      return(list(type="TimeStamp", coordinates=start))
    }
    return(list(type="TimeInterval", coordinates=list(start, end)))
  }
  if (is.infinite(high)) {
    high <- recording$samplerate / 2
  }
  if (start == end && low == high) {
    return(list(type="Point", coordinates=list(start, low)))
  }
  return(list(type="BoundingBox", coordinates=list(start, low, end, high)))
}

#' The tags of the annotations of one object
#'
#' @param tags An environment of the tags given out so far, as their ids
#'   named by key and value.
#' @param annotations The annotations.
#' @return The ids of their tags.
#' @keywords internal
#' @noRd
.aoefTags <- function(tags, annotations) {
  ids <- integer(0)
  for (a in annotations) {
    type <- if (is.na(a@type)) "" else a@type
    value <- if (is.na(a@value)) "" else a@value
    if (!nzchar(type) && !nzchar(value)) {
      next
    }
    if (!nzchar(type)) {
      type <- "label"
    }
    #A carriage return is in neither a key nor a value Whombat lets anybody
    #type, so it parts them.
    k <- paste(type, value, sep="\r")
    if (is.null(tags[[k]])) {
      assign(k, length(ls(tags, all.names=TRUE, sorted=FALSE)), envir=tags)
    }
    ids <- c(ids, tags[[k]])
  }
  return(unique(ids))
}

#' The identifier of a person who made an annotation
#'
#' @param users An environment of the identifiers given out, by name.
#' @param issued An environment of all identifiers given out.
#' @param name The name of the person.
#' @param uuid The identifier readAOEF() kept, or NULL.
#' @return An identifier.
#' @keywords internal
#' @noRd
.aoefUser <- function(users, issued, name, uuid) {
  if (is.null(users[[name]])) {
    assign(name, .aoefClaim(issued, uuid), envir=users)
  }
  return(users[[name]])
}

#' Notes as AOEF writes them
#'
#' @param issued An environment of the identifiers given out.
#' @param notes A character vector of notes, or NULL.
#' @return A list of notes, or NULL.
#' @keywords internal
#' @noRd
.aoefNotes <- function(issued, notes) {
  if (!is.character(notes) || length(notes) == 0) {
    return(NULL)
  }
  return(lapply(notes, function(n) list(uuid=.aoefClaim(issued, NULL), message=n, is_issue=FALSE)))
}

#' Features as AOEF writes them
#'
#' @param features A named numeric vector, or NULL.
#' @return A named list, or NULL.
#' @keywords internal
#' @noRd
.aoefFeatures <- function(features) {
  if (!is.numeric(features) || length(features) == 0 || is.null(names(features))) {
    return(NULL)
  }
  keep <- is.finite(features) & !is.na(names(features)) & nzchar(names(features))
  if (!any(keep)) {
    return(NULL)
  }
  return(as.list(features[keep]))
}

#' A time as AOEF writes it
#'
#' AOEF holds a date and a time of day, so a date alone is not written, rather
#' than being given a time it does not have; nor is an empty one. A time is
#' text, as readAOEF() and the `dateIdentified` of an annotation give it.
#' @param time A time as ISO 8601 text.
#' @return Text, or NULL.
#' @keywords internal
#' @noRd
.aoefTime <- function(time) {
  if (is.character(time) && length(time) == 1 && !is.na(time) &&
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", time)) {
    return(time)
  }
  return(NULL)
}

#' Write a list as JSON
#'
#' A list with names is written as an object and one without as an array, and
#' a vector of one value as that value. A field of an object that holds
#' nothing is left out, as AOEF leaves it out.
#' @param x A list, or a value.
#' @return JSON text.
#' @keywords internal
#' @noRd
.aoefJSON <- function(x) {
  return(.aoefJSONs(list(x)))
}

#' Write several values as JSON, each on its own
#'
#' Written with jsonlite, a project of a few thousand sound events took close
#' to a minute, nearly all of it spent choosing how to write each of its many
#' small lists, and written one value at a time it took much of that. So the
#' values are written a kind at a time: every string at every level of an
#' array of objects is quoted in one call, every number written in one, and
#' every object of the array is put together a field at a time.
#' @param values A list of values.
#' @return A character vector of JSON texts.
#' @keywords internal
#' @noRd
.aoefJSONs <- function(values) {
  kind <- vapply(values, function(v) {
    if (is.null(v)) {
      return("null")
    }
    if (is.list(v)) {
      return(if (is.null(names(v))) "array" else "object")
    }
    if (length(v) != 1) {
      return("vector")
    }
    if (is.na(v)) {
      return("null")
    }
    return(typeof(v))
  }, character(1), USE.NAMES=FALSE)
  out <- rep("null", length(values))

  #A vector of other than one value is an array of its values.
  values[kind == "vector"] <- lapply(values[kind == "vector"], function(v) as.list(unname(v)))
  kind[kind == "vector"] <- "array"

  i <- kind == "character"
  if (any(i)) {
    out[i] <- .aoefQuote(unlist(values[i]))
  }
  i <- kind == "double"
  if (any(i)) {
    numbers <- unlist(values[i])
    if (!all(is.finite(numbers))) {
      stop("A value of ", numbers[!is.finite(numbers)][1], " cannot be written in AOEF.", call.=FALSE)
    }
    out[i] <- .aoefNumber(numbers)
  }
  i <- kind == "integer"
  out[i] <- as.character(unlist(values[i]))
  i <- kind == "logical"
  out[i] <- c("false", "true")[unlist(values[i]) + 1]

  i <- which(kind == "array")
  if (length(i) > 0) {
    arrays <- values[i]
    texts <- .aoefJSONs(unlist(arrays, recursive=FALSE, use.names=FALSE))
    owner <- factor(rep(seq_along(arrays), lengths(arrays)), levels=seq_along(arrays))
    out[i] <- paste0("[", vapply(split(texts, owner), paste, character(1), collapse=",", USE.NAMES=FALSE), "]")
  }

  i <- which(kind == "object")
  if (length(i) > 0) {
    objects <- values[i]
    body <- character(length(objects))
    for (field in unique(unlist(lapply(objects, names), use.names=FALSE))) {
      column <- lapply(objects, `[[`, field)
      present <- lengths(column) > 0
      body[present] <- paste0(body[present], .aoefQuote(field), ":", .aoefJSONs(column[present]), ",")
    }
    out[i] <- paste0("{", sub(",$", "", body), "}")
  }
  return(out)
}

#' Text as a JSON string
#'
#' Characters outside ASCII are left for .aoefAscii(), which escapes them in
#' the whole document at once.
#' @param x A character vector.
#' @return Each element as a JSON string.
#' @keywords internal
#' @noRd
.aoefQuote <- function(x) {
  x <- enc2utf8(x)
  special <- grepl("[\"\\\\\001-\037]", x, perl=TRUE)
  if (any(special)) {
    s <- gsub("\\", "\\\\", x[special], fixed=TRUE)
    s <- gsub("\"", "\\\"", s, fixed=TRUE)
    controls <- gregexpr("[\001-\037]", s, perl=TRUE)
    regmatches(s, controls) <- lapply(regmatches(s, controls), function(found) {
      sprintf("\\u%04x", vapply(found, utf8ToInt, integer(1), USE.NAMES=FALSE))
    })
    x[special] <- s
  }
  return(paste0("\"", x, "\""))
}

#' A number written so that it reads back as it was
#'
#' jsonlite writes a number to at most 15 significant digits, so a time read
#' from a file came back changed in its last places. Each number is written
#' here in the fewest digits that give it back exactly, as Python writes them,
#' so that 0.1 is still written as 0.1.
#' @param x Finite numbers.
#' @return Their text.
#' @keywords internal
#' @noRd
.aoefNumber <- function(x) {
  text <- sprintf("%.15g", x)
  for (digits in 16:17) {
    inexact <- as.numeric(text) != x
    text[inexact] <- sprintf(paste0("%.", digits, "g"), x[inexact])
  }
  return(text)
}

#' JSON in ASCII alone
#'
#' Every character outside ASCII is written as an escape, so that a reader
#' gets it right whatever encoding it takes the file to be in: soundevent on
#' Windows reads in the encoding of the locale.
#' @param json JSON text.
#' @return The text, in ASCII.
#' @keywords internal
#' @noRd
.aoefAscii <- function(json) {
  json <- enc2utf8(as.character(json))
  #Only the characters outside ASCII are looked at, as a file can be large.
  found <- gregexpr("[^\\x{00}-\\x{7f}]", json, perl=TRUE)
  if (found[[1]][1] == -1) {
    return(json)
  }
  regmatches(json, found) <- list(vapply(regmatches(json, found)[[1]], function(ch) {
    p <- utf8ToInt(ch)
    if (p < 65536L) {
      return(sprintf("\\u%04x", p))
    }
    #Beyond the first plane a character is written as a pair of surrogates.
    p <- p - 65536L
    return(sprintf("\\u%04x\\u%04x", 55296L + p %/% 1024L, 56320L + p %% 1024L))
  }, character(1), USE.NAMES=FALSE))
  return(json)
}
