# The XMP packet of a wave file, which RIFF carries in a chunk named _PMX.
#
# XMP is Adobe's metadata format: an RDF document, in UTF-8, describing the
# file. Audio software writes the technical description of the recording there
# along with whatever the editor was told about it, and because RDF allows a
# property to be written as an attribute, as an element, or as a list of
# alternatives, the same packet can say the same kind of thing in three shapes.

#' The attributes of an RDF document that describe the document rather than
#' the file it is about
#' @keywords internal
#' @noRd
.XMP_STRUCTURAL <- c(
  "rdf:about", "rdf:parseType", "rdf:resource", "rdf:datatype", "rdf:nodeID",
  "rdf:ID", "xml:lang", "xml:base"
)

#' Read the XMP metadata of a wave file
#'
#' Reads the `_PMX` chunk of a wave file, the chunk RIFF carries an XMP packet
#' in. XMP is the metadata format of Adobe's audio software, and a file that
#' has passed through an editor commonly holds the sample rate, sample type,
#' duration, tempo and key the editor worked out, beside the title, artist,
#' description and rights it was told.
#'
#' Every property of the document is read, named as the document names it, so
#' that properties this package has never heard of are read as readily as the
#' ones it has. A property written as an attribute, as an element, and as a
#' list of alternatives all read the same way; where a list holds one entry
#' marked as the default language that entry is taken, and where it holds
#' several they are all given. A property named more than once gives `name`,
#' `name.2` and so on. The packet itself is kept on the result as its "xml"
#' attribute.
#'
#' @param x Path to a wave file, a Wave object with XMP metadata attached, a
#'   raw vector holding a wave file, or an xmp object
#' @return An xmp object, which is a named list of properties
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once.
#' @examples
#' \dontrun{
#' xmp("recording.wav")
#' }
#'
xmp <- function(x) {
  return(.mdDispatch(
    x, "xmp",
    fromFile=function(file) .mdXmpObject(.mdWavChunk(file, "_PMX")),
    fromBytes=function(bytes) .mdXmpObject(.mdWavChunkBytes(bytes, "_PMX"))
  ))
}

#' Build an xmp object from the body of a _PMX chunk
#' @keywords internal
#' @noRd
.mdXmpObject <- function(body) {
  if (is.null(body) || length(body) == 0L) {
    return(.mdObject(list(), "xmp"))
  }
  # The packet declares itself to be UTF-8, and a writer pads it with spaces so
  # that a later one can rewrite it in place without moving the audio.
  text <- trimws(.mdDecode(body, from="UTF-8"))
  if (!nzchar(text)) {
    return(.mdObject(list(), "xmp"))
  }
  fields <- list()
  add <- function(name, value) {
    value <- trimws(.mdUnescapeXml(value))
    if (length(value) == 1L && nzchar(value)) {
      fields[[.mdUniqueName(fields, name)]] <<- value
    }
  }

  # A property whose value is a list of alternatives, of unordered entries, or
  # of ordered ones. These are read first and then taken out of the document,
  # so that the pass over single elements below sees the properties themselves
  # rather than the rdf:li entries inside them.
  rest <- text
  containers <- regmatches(
    rest,
    gregexpr(
      paste0(
        "<([A-Za-z_][A-Za-z0-9_.:-]*)(?:[[:space:]][^<>]*)?>[[:space:]]*",
        "<rdf:(?:Alt|Bag|Seq)>(?s:.*?)</rdf:(?:Alt|Bag|Seq)>[[:space:]]*</\\1>"
      ),
      rest,
      perl=TRUE
    )
  )[[1]]
  for (one in containers) {
    tag <- sub("^<([A-Za-z_][A-Za-z0-9_.:-]*).*$", "\\1", one)
    entries <- regmatches(
      one,
      gregexpr("<rdf:li(?:[[:space:]][^<>]*)?>[^<>]*</rdf:li>", one, perl=TRUE)
    )[[1]]
    values <- sub("^<[^>]*>(.*)</rdf:li>$", "\\1", entries)
    # A title written for several languages names one of them the default, and
    # that is the one to report. Where nothing is marked, every entry is.
    default <- grepl("xml:lang=\"x-default\"", entries, fixed=TRUE)
    if (any(default)) {
      values <- values[default][1]
    }
    for (value in values) {
      add(tag, value)
    }
    rest <- sub(one, "", rest, fixed=TRUE)
  }

  # A property written as an element holding nothing but its value.
  leaves <- regmatches(
    rest,
    gregexpr(
      "<([A-Za-z_][A-Za-z0-9_.:-]*)(?:[[:space:]][^<>]*)?>[^<>]*</\\1>",
      rest,
      perl=TRUE
    )
  )[[1]]
  for (one in leaves) {
    tag <- sub("^<([A-Za-z_][A-Za-z0-9_.:-]*).*$", "\\1", one)
    # An entry of a list whose own property was written in a shape this did not
    # match would otherwise be reported under the name of the entry.
    if (identical(tag, "rdf:li")) {
      next
    }
    add(tag, sub("^<[^>]*>(.*)</[^>]*>$", "\\1", one))
  }

  # A property written as an attribute of the description it belongs to.
  attributes <- regmatches(
    rest,
    gregexpr(
      "[A-Za-z_][A-Za-z0-9_.-]*:[A-Za-z0-9_.-]+=\"[^\"]*\"",
      rest,
      perl=TRUE
    )
  )[[1]]
  for (one in attributes) {
    name <- sub("=.*$", "", one)
    # A namespace declaration says where the names came from, and the
    # structural attributes describe the document, not the recording.
    if (grepl("^xmlns:", name) || name %in% .XMP_STRUCTURAL) {
      next
    }
    add(name, sub("^[^=]*=\"(.*)\"$", "\\1", one))
  }

  return(structure(.mdObject(fields, "xmp"), xml=text))
}
