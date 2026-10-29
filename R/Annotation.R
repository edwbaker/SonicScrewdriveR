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
#' @param annotation An Annotation object.
#' @param wave (Optional) A Wave object, if not given will load the wave file from Annotation filename.
#' @importFrom tuneR writeWave
#' @importFrom tools  file_path_sans_ext
#' @return No return value, called for its side effect of writing a Wave file.
#' @export
writeAnnotationWave <- function(annotation, wave=NULL) {
  if (is.list(annotation)) {
    lapply(annotation, writeAnnotationWave, wave=wave)
    return()
  }
  if (is.null(wave)) {
    wave <- readWave(annotation@file)
  }

  output <- paste0(file_path_sans_ext(basename(annotation@file)), "_", annotation@start, "-", annotation@end, ".wav")

  end <- annotation@end
  if (duration(wave) < annotation@end) {
    message("Annotation end time is greater than the duration of the wave file.")
    end <- duration(wave)
  }
  region <- cutw(wave, from=annotation@start, to=end, unit="seconds", output="Wave")
  writeWave(region, filename=output)
}

#' Check if two annotations overlap or are continuous
#' @param annotation1 An Annotation object.
#' @param annotation2 An Annotation object.
#' @param domain Domain of the annotations, either "time", "frequency", or "both".
#' @return TRUE if the annotations overlap, FALSE otherwise.
.annotation_check_overlap <- function(annotation1, annotation2, domain="time") {
  if (domain == "time") {
    if (annotation1@start <= annotation2@end && annotation1@end >= annotation2@start) {
      return(TRUE)
    }
    return(FALSE)
  }
  if (domain == "frequency") {
    if (annotation1@low <= annotation2@high && annotation1@high >= annotation2@low) {
      return(TRUE)
    }
    return(FALSE)
  }
  if (domain == "both") {
    if (
      annotation1@start <= annotation2@end && annotation1@end >= annotation2@start &&
      annotation1@low <= annotation2@high && annotation1@high >= annotation2@low
    ) {
      return(TRUE)
    }
    return(FALSE)
  }
}

#' Combine annotations
#'
#' Checks a list of annotations for those that are overlapping in the time or
#' frequency domain, and returns a list where overlapping annotations are merged.
#' Annotation objects must have the same `file`, `type` and `value` to be merged.
#' @param annotations A list of Annotation objects.
#' @param domain Domain of the annotations, either "time" or "frequency".
#' @param same.source If TRUE, annotations must have the same source to be merged.
#' @return A list of Annotation objects.
#' @export
merge_annotations <- function(annotations, domain="time", same.source=TRUE) {
  ret <- list()
  if (length(annotations) == 0) {
    return(ret)
  }

  if (same.source) {
    rel_columns <- sapply(annotations, function(x) list(file=x@file, type=x@type, value=x@value, source=x@source))
    uniq_cols <- unique(as.data.frame(t(rel_columns)))

    for (i in seq_len(nrow(uniq_cols))) {
      file <- uniq_cols[i, "file"]
      type <- uniq_cols[i, "type"]
      value <- uniq_cols[i, "value"]
      source <- uniq_cols[i, "source"]
      filtered <- sapply(annotations, function(x) {
        x@file == file & x@type == type & x@value == value & x@source == source
      })
      ret <- c(ret, .merge_annotations(annotations[filtered], domain=domain))
    }
  } else {
    rel_columns <- sapply(annotations, function(x) list(file=x@file, type=x@type, value=x@value))
    uniq_cols <- unique(as.data.frame(t(rel_columns)))

    for (i in seq_len(nrow(uniq_cols))) {
      file <- uniq_cols[i, "file"]
      type <- uniq_cols[i, "type"]
      value <- uniq_cols[i, "value"]
      filtered <- sapply(annotations, function(x) {
        x@file == file & x@type == type & x@value == value
      })
      ret <- c(ret, .merge_annotations(annotations[filtered], domain=domain))
    }
  }

  return(ret)
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
  if (domain == "time") {
    annotations <- sort_annotations(annotations)
    if (length(annotations) < 2) {
      return(annotations)
    }
    remove = vector(mode="logical", length=length(annotations))
    for (i in 1:(length(annotations)-1)) {
      if (.annotation_check_overlap(annotations[[i]], annotations[[i+1]], domain=domain)) {
        #The merged annotation spans both, so it keeps the earlier start and
        #whichever end is later. Taking only the start discarded the coverage of
        #an annotation that wholly contained the next one.
        annotations[[i+1]]@start <- annotations[[i]]@start
        annotations[[i+1]]@end <- max(annotations[[i]]@end, annotations[[i+1]]@end)
        remove[i] <- TRUE
      }
    }
    annotations <- annotations[!remove]
    return(annotations)
  }
  if (domain == "frequency") {
    annotations <- sort_annotations(annotations, domain="frequency")
    if (length(annotations) < 2) {
      return(annotations)
    }
    remove = vector(mode="logical", length=length(annotations))
    for (i in 1:(length(annotations)-1)) {
      if (.annotation_check_overlap(annotations[[i]], annotations[[i+1]], domain=domain)) {
        annotations[[i+1]]@low <- annotations[[i]]@low
        annotations[[i+1]]@high <- max(annotations[[i]]@high, annotations[[i+1]]@high)
        remove[i] <- TRUE
      }
    }
    annotations <- annotations[!remove]
    return(annotations)
  }
  stop(paste("Unknown domain for merge_annotations:", domain))
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
  missing <- vapply(values, is.null, logical(1))
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
