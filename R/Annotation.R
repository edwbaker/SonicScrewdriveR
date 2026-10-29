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
#' @param x A list of Annotation objects.
#' @return A data.frame.
#' @export
AnnotationList2DataFrame <- function(x) {
  if (!all(sapply(x, inherits, "Annotation"))) {
    stop("All items in list must be Annotation objects.")
  }

  # Get all possible names of values in the metadata slot
  metadata_names <- unique(unlist(lapply(x, function(y) names(y@metadata))))

  ret <- data.frame(
    file=character(length=length(x)),
    start=numeric(length=length(x)),
    end=numeric(length=length(x)),
    low=numeric(length=length(x)),
    high=numeric(length=length(x)),
    source=character(length=length(x)),
    type=character(length=length(x)),
    value=character(length=length(x)),
    stringsAsFactors=FALSE
  )

  names <- colnames(ret)

  for (i in seq_along(metadata_names)) {
    ret <- cbind(ret, character(length=length(x)))
  }
  colnames(ret) <- c(names, metadata_names)

  for (i in seq_along(x)) {
    ret[i, "file"] <- x[[i]]@file
    ret[i, "start"] <- x[[i]]@start
    ret[i, "end"] <- x[[i]]@end
    ret[i, "low"] <- x[[i]]@low
    ret[i, "high"] <- x[[i]]@high
    ret[i, "source"] <- x[[i]]@source
    ret[i, "type"] <- x[[i]]@type
    ret[i, "value"] <- x[[i]]@value

    for (j in seq_along(metadata_names)) {
      if (metadata_names[j] %in% names(x[[i]]@metadata)) {
        ret[i, metadata_names[j]] <- x[[i]]@metadata[[metadata_names[j]]]
      }
    }
  }
  return(ret)
}
