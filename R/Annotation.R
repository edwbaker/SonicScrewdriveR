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
