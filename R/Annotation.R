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
    file = NA_character_,
    metadata = list(),
    start = 0,
    end = Inf,
    low = 0,
    high = Inf,
    source = NA_character_,
    type = NA_character_,
    value = NA_character_
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
  file=NA_character_,
  metadata=list(),
  start=0,
  end=Inf,
  low=0,
  high=Inf,
  source=NA_character_,
  type=NA_character_,
  value=NA_character_
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

#' Check if two annotations can be merged
#' @param annotation1 An Annotation object.
#' @param annotation2 An Annotation object.
#' @param same.source If TRUE, annotations must have the same source to be merged.
#' @return TRUE if the annotations can be merged, FALSE otherwise.
.annotation_can_merge <- function(annotation1, annotation2, same.source=TRUE) {
  if (is.na(annotation1@source) & is.na(annotation2@source)) {
    if (is.na(annotation1@type) & is.na(annotation2@type)) {
      if (is.na(annotation1@value) & is.na(annotation2@value)) {
        return(TRUE)
      } else {
        if (annotation1@value != annotation2@value) {
          return(FALSE)
        }
      }
    } else {
      if (annotation1@type != annotation2@type) {
        return(FALSE)
      }
    }
  } else {
    if (same.source & annotation1@source != annotation2@source) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Combine overlapping annotations
#' @param annotation1 An Annotation object.
#' @param annotation2 An Annotation object.
#' @param domain Domain of the annotations, either "time", "frequency", or "both".
#' @param same.source If TRUE, annotations must have the same source to be merged.
#' @return A new Annotation object.
.annotation_merge_overlapping <- function(annotation1, annotation2, domain="time", same.source=TRUE) {
  if (!.annotation_can_merge(annotation1, annotation2, same.source=same.source)) {
    return(FALSE)
  }
  if (!.annotation_check_overlap(annotation1, annotation2, domain=domain)) {
    return(FALSE)
  }
  start <- 0
  end <- Inf
  low <- 0
  high <- Inf

  if (domain == "time" | domain =="both") {
    start = min(annotation1@start, annotation2@start)
    end = max(annotation1@end, annotation2@end)
  }
  if (domain == "frequency" | domain =="both") {
    low = min(annotation1@low, annotation2@low)
    high = max(annotation1@high, annotation2@high)
  }

  new_annotation <- annotation(
    file=annotation1@file,
    metadata=c(annotation1@metadata, annotation2@metadata),
    start=start,
    end=end,
    low=low,
    high=high,
    source=annotation1@source,
    type=annotation1@type,
    value=annotation1@value
  )
  return(new_annotation)
}

#' Combine annotations
#'
#' Checks a list of annotations for those that are overlapping, and returns a list
#' where overlapping annotations are merged.
#' @param annotations A list of Annotation objects.
#' @param domain Domain of the annotations, either "time", "frequency", or "both".
#' @param same.source If TRUE, annotations must have the same source to be merged.
#' @return A list of Annotation objects.
#' @export
annotations_merge <- function(annotations, domain="time", same.source=TRUE) {
  if (length(annotations) < 2) {
    return(annotations)
  }
  annotations <- sort_annotations(annotations, domain=domain)
  merged <- list(annotations[[1]])
  for (i in 1:length(annotations)) {
    for (j in 1:length(merged)) {
      merged_annotation <- .annotation_merge_overlapping(merged[[j]], annotations[[i]], domain=domain, same.source=same.source)
      if (is.logical(merged_annotation)) {
        merged <- c(merged, annotations[i])
      } else {
        merged[[j]] <- merged_annotation
        break
      }
    }
  }
  return(merged)
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
