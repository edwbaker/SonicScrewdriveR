#' A S4 class for annotations
#'
#' The `Annotation` class is used to store annotations on `Wave`-like objects.
#'
#' @slot file File being annotated.
#' @slot metadata A list for storing metadata.
#' @slot start Start time of annotation.
#' @slot end End time of annotation.
#' @slot low Low frequency of annotation.
#' @slot high High frequency of annotation.
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

  if (same.source) {
    rel_columns <- sapply(annotations, function(x) list(file=x@file, type=x@type, value=x@value, source=x@source))
    uniq_cols <- unique(as.data.frame(t(rel_columns)))

    for (i in 1:nrow(uniq_cols)) {
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

    for (i in 1:nrow(uniq_cols)) {
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
        annotations[[i+1]]@start <- annotations[[i]]@start
        remove[i] <- TRUE
      }
    }
    annotations <- annotations[!remove]
    return(annotations)
  }
  if (domain == "frequency") {
    annotations <- sort_annotations(annotations, domain="frequency")
    if (length(annotations) == 1) {
      return(annotations)
    }
    remove = vector(mode="logical", length=length(annotations))
    for (i in 1:(length(annotations)-1)) {
      if (.annotation_check_overlap(annotations[[i]], annotations[[i+1]], domain=domain)) {
        annotations[[i+1]]@low <- annotations[[i]]@low
        remove[i] <- TRUE
      }
    }
    annotations <- annotations[!remove]
    return(annotations)
  }
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
