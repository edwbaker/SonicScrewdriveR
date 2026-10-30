#' A S4 class for annotations
#'
#' The `Annotation` class is used to store annotations on `Wave`-like objects.
#'
#' @slot file File being annotated.
#' @slot metadata A list for storing metadata.
#' @slot start Start time of annotation.
#' @slot end End time of annotation.
#' @slot low Low frequency of annotation (Hz).
#' @slot high High frequency of annotation (Hz).
#' @slot source Source of annotation.
#' @slot type Type of annotation.
#' @slot value Value of annotation.
setClass(
  "Annotation",
  slots=list(
    file="character",
    metadata="list",
    start="numeric",
    end="numeric",
    low="numeric",
    high="numeric",
    source="character",
    type="character",
    value="character"
  ),
  prototype = list(
    file = "",
    metadata = list(),
    start = 0,
    end = Inf,
    low = 0,
    high = Inf,
    source = "",
    type = "",
    value = ""
  )
)

#' Create a new Annotation object
#'
#' @param file File being annotated.
#' @param metadata A list of metadata.
#' @param start Start time of annotation (seconds).
#' @param end End time of annotation (seconds).
#' @param low Low frequency of annotation (Hz).
#' @param high High frequency of annotation (Hz).
#' @param source Source of annotation.
#' @param type Type of annotation.
#' @param value Value of annotation.
#' @return An Annotation object.
#' @export
annotation <- function(
  file="",
  metadata=list(),
  start=0,
  end=Inf,
  low=0,
  high=Inf,
  source="",
  type="",
  value=""
) {
  annotation <- new(
    "Annotation",
    file=as.character(file),
    metadata=as.list(metadata),
    start=as.numeric(start),
    end=as.numeric(end),
    low=as.numeric(low),
    high=as.numeric(high),
    source=as.character(source),
    type=as.character(type),
    value=as.character(value)
  )
  return(annotation)
}

#' Save a time region defined by an Annotation object as a Wave file
#'
#' The region is cut from the wave by [cutws()], so a stereo or multichannel
#' wave keeps all of its channels and its bit depth.
#'
#' Each region is written to `dir`, in a file named from the annotated file, the
#' start and end of the annotation and, where it has one, its value, as
#' `AUDIOMOTH_0-3_Turdus_merula.wav`. Characters of the value other than
#' letters, digits, dots, dashes and underscores are written as underscores.
#' Two annotations of the same region, such as two species found in the same
#' window by [birdNetAnalyse()], are written to two files.
#'
#' @param annotation An Annotation object, or a list of them.
#' @param wave (Optional) A Wave object, if not given will load the wave file from Annotation filename.
#' @param dir The directory to write the files to.
#' @importFrom tuneR writeWave
#' @importFrom tools  file_path_sans_ext
#' @return Invisibly, the path of each file written, or `NA` for an annotation
#'   whose region holds none of the wave.
#' @export
writeAnnotationWave <- function(annotation, wave=NULL, dir=".") {
  if (is.list(annotation)) {
    return(invisible(vapply(annotation, writeAnnotationWave, character(1), wave=wave, dir=dir)))
  }
  if (is.null(wave)) {
    wave <- readWave(annotation@file)
  }

  name <- paste0(file_path_sans_ext(basename(annotation@file)), "_", annotation@start, "-", annotation@end)
  if (!is.na(annotation@value) && nzchar(annotation@value)) {
    name <- paste0(name, "_", gsub("[^A-Za-z0-9._-]", "_", annotation@value, perl=TRUE))
  }
  output <- file.path(dir, paste0(name, ".wav"))

  #Counted rather than asked of seewave's duration(), which refuses a tagged wave.
  seconds <- length(wave) / wave@samp.rate
  end <- annotation@end
  if (!is.na(end) && seconds < end) {
    #An end of Inf is an annotation running to the end of the file, as it says.
    if (is.finite(end)) {
      message("Annotation end time is greater than the duration of the wave file.")
    }
    end <- seconds
  }
  #Cut by samples, keeping every channel: seewave's cutw() kept only the first.
  from <- max(round(annotation@start * wave@samp.rate) + 1, 1)
  to <- round(end * wave@samp.rate)
  if (is.na(from) || is.na(to) || from > to) {
    message("Annotation holds none of the wave, so nothing is written for it.")
    return(invisible(NA_character_))
  }
  writeWave(cutws(wave, from=from, to=to), filename=output)
  return(invisible(output))
}

#' Check if two annotations overlap or are continuous
#' @param annotation1 An Annotation object.
#' @param annotation2 An Annotation object.
#' @param domain Domain of the annotations, either "time", "frequency", or "both".
#' @return TRUE if the annotations overlap, FALSE otherwise. An annotation whose
#'   limits in the domain are not known, being `NA`, overlaps nothing.
.annotation_check_overlap <- function(annotation1, annotation2, domain="time") {
  .validateChoice(domain, c("time", "frequency", "both"), msg=paste("Unknown domain:", domain))
  time <- annotation1@start <= annotation2@end && annotation1@end >= annotation2@start
  frequency <- annotation1@low <= annotation2@high && annotation1@high >= annotation2@low
  #A comparison with NA is NA, which if() cannot take.
  return(isTRUE(switch(domain, time=time, frequency=frequency, both=time && frequency)))
}

#' Combine annotations
#'
#' Checks a list of annotations for those that are overlapping in the time or
#' frequency domain, and returns a list where overlapping annotations are merged.
#' Annotation objects must have the same `file`, `type` and `value` to be merged,
#' where `NA` is the same as `NA` and nothing else.
#'
#' The merged annotation spans all of those merged into it, in both domains. It
#' keeps the `source` and `metadata` of the last of them in the order
#' [sort_annotations()] puts them in.
#' @param annotations A list of Annotation objects.
#' @param domain Domain of the annotations, either "time" or "frequency".
#' @param same.source If TRUE, annotations must have the same source to be merged.
#' @return A list of Annotation objects.
#' @export
merge_annotations <- function(annotations, domain="time", same.source=TRUE) {
  if (length(annotations) == 0) {
    return(list())
  }
  #Comparing the slots with == gave NA for a slot holding NA, which took the
  #annotation out of every group. Quoted, NA is apart from the text "NA".
  key <- vapply(annotations, function(x) {
    slots <- c(x@file, x@type, x@value, if (same.source) x@source)
    paste(encodeString(slots, quote='"'), collapse=" ")
  }, character(1))
  groups <- split(annotations, factor(key, levels=unique(key)))
  return(do.call(c, unname(lapply(groups, .merge_annotations, domain=domain))))
}

#' Combine annotations helper function
#'
#' Checks a list of annotations for those that are overlapping in the time or
#' frequency domain, and returns a list where overlapping annotations are merged.
#'
#' The exported function `merge_annotations()` handles sanity checks and calls this function.
#' @param annotations A list of Annotation objects.
#' @param domain Domain of the annotations, either "time" or "frequency".
#' @return A list of Annotation objects.
.merge_annotations <- function(annotations, domain="time") {
  .validateChoice(domain, c("time", "frequency"), msg=paste("Unknown domain for merge_annotations:", domain))
  annotations <- sort_annotations(annotations, domain=domain)
  if (length(annotations) < 2) {
    return(annotations)
  }
  remove = vector(mode="logical", length=length(annotations))
  for (i in 1:(length(annotations)-1)) {
    if (.annotation_check_overlap(annotations[[i]], annotations[[i+1]], domain=domain)) {
      annotations[[i+1]] <- .annotation_span(annotations[[i]], annotations[[i+1]])
      remove[i] <- TRUE
    }
  }
  annotations <- annotations[!remove]
  return(annotations)
}

#' The smallest annotation holding two others
#'
#' The merged annotation spans both in time and in frequency. Taking only the
#' start of the earlier one discarded the coverage of an annotation that wholly
#' contained the next, and taking the limits of the other domain from the later
#' one alone discarded those of the earlier.
#' @param earlier An Annotation object.
#' @param later An Annotation object, whose other slots are kept.
#' @return An Annotation object.
#' @keywords internal
#' @noRd
.annotation_span <- function(earlier, later) {
  later@start <- min(earlier@start, later@start)
  later@end <- max(earlier@end, later@end)
  later@low <- min(earlier@low, later@low)
  later@high <- max(earlier@high, later@high)
  return(later)
}

#' Sort annotations
#'
#' Sorts a list of annotations by either start time, frequency, or both.
#' @param annotations A list of Annotation objects.
#' @param domain Domain of the annotations, either "time", "frequency", or "both".
#' @param decreasing If TRUE, sort in decreasing order.
#' @return A list of Annotation objects.
#' @export
sort_annotations <- function(annotations, domain="time", decreasing=FALSE) {
  .validateChoice(domain, c("time", "frequency", "both"), msg=paste("Unknown domain for sort_annotations:", domain))
  if (domain == "frequency" | domain == "both") {
    annotations <- annotations[order(sapply(annotations, function(x) x@low))]
  }
  if (domain == "time" | domain == "both") {
    annotations <- annotations[order(sapply(annotations, function(x) x@start))]
  }

  if (decreasing) {
    annotations <- rev(annotations)
  }
  return(annotations)
}

#' Convert a list of Annotation objects to a data.frame
#'
#' Converts a list of Annotation objects to a data.frame, with one column for
#' each metadata name in the list.
#'
#' A metadata column keeps the class of its values where they share one, so a
#' confidence is a number; values of differing classes are given as text, and
#' a column in which any value is longer than one is a list. An annotation
#' without a value of that name has `NA`. A metadata name that is also the name
#' of a slot, such as the `source` that [audioblast()] keeps of the collection
#' an annotation came from, is given a column of its own named with `metadata.`
#' in front, rather than writing over the slot's.
#'
#' @param x A list of Annotation objects.
#' @return A data.frame.
#' @importFrom methods slot
#' @export
AnnotationList2DataFrame <- function(x) {
  if (!all(sapply(x, inherits, "Annotation"))) {
    stop("All items in list must be Annotation objects.")
  }

  #Filled a column at a time: filling a data frame a cell at a time copied it
  #on every assignment.
  slot_column <- function(name, type) {
    vapply(x, function(y) slot(y, name), type)
  }
  ret <- data.frame(
    file=slot_column("file", character(1)),
    start=slot_column("start", numeric(1)),
    end=slot_column("end", numeric(1)),
    low=slot_column("low", numeric(1)),
    high=slot_column("high", numeric(1)),
    source=slot_column("source", character(1)),
    type=slot_column("type", character(1)),
    value=slot_column("value", character(1)),
    stringsAsFactors=FALSE
  )

  # Get all possible names of values in the metadata slot
  metadata_names <- unique(unlist(lapply(x, function(y) names(y@metadata))))
  metadata_names <- metadata_names[nzchar(metadata_names)]
  columns <- ifelse(metadata_names %in% colnames(ret), paste0("metadata.", metadata_names), metadata_names)
  columns <- make.unique(c(colnames(ret), columns))[-seq_len(ncol(ret))]

  for (j in seq_along(metadata_names)) {
    ret[[columns[j]]] <- .metadataColumn(lapply(x, function(y) y@metadata[[metadata_names[j]]]))
  }
  return(ret)
}

#' One metadata column of AnnotationList2DataFrame()
#'
#' Every value was written into a column of text, so a confidence came back as
#' "0.9".
#' @param values A list of the values of one metadata name, with NULL for an
#'   annotation that has none.
#' @return A vector, or a list where any value is longer than one.
#' @keywords internal
#' @noRd
.metadataColumn <- function(values) {
  #A plain NA is missing as NULL is, so that NA among numbers is not taken for
  #a value of a second class and the column given as text.
  missing <- vapply(values, function(v) {
    is.null(v) || (is.logical(v) && length(v) == 1L && is.na(v))
  }, logical(1))
  if (all(missing)) {
    return(rep(NA, length(values)))
  }
  single <- vapply(values, function(v) is.atomic(v) && length(v) == 1L, logical(1))
  if (!all(missing | single)) {
    return(I(values))
  }
  if (length(unique(lapply(values[!missing], class))) > 1) {
    values[!missing] <- lapply(values[!missing], as.character)
  }
  #An NA of the class of the values, so that combining them keeps the class
  values[missing] <- list(values[!missing][[1]][NA_integer_])
  return(do.call(c, unname(values)))
}
