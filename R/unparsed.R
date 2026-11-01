# What a reader reports when it has not understood what it was given.
#
# There are three things a reader of this package can mean by "no value", and
# they are three different answers:
#
#   1. The file does not carry the field. The normalised record of
#      readMetadata() simply does not mention it — .mdMergeFields() drops a
#      NULL, and .mdNormaliseFilename() gives the reasoning: reporting it would
#      "put a field in the record saying the recording has a date and that the
#      date is not known, which is a worse answer than the record simply not
#      mentioning one". A reading of one source has a fixed shape, so there the
#      field is NA.
#
#   2. The file carries the field as a placeholder for a value its own writer
#      never had. xeno-canto writes a time of `?h`, an elevation of `?m`, a
#      date with a zero month, rather than leaving the field out. Read as an
#      absence, which is to say exactly as 1: for a caller, "the collection was
#      never told" and "the field is not there" are the same answer, and the
#      reading keeps the text it was read from for anyone auditing the file.
#
#   3. The file carries the field in a form this package cannot read: a
#      recorder's wording that no pattern here matches, a filename whose digits
#      spell no real date. The value is NA, and the field is named in the
#      reading's `unparsed`.
#
# From which the rule, which is the whole of it:
#
#   Never report a default as a reading. A value that was not read is NA, never
#   FALSE, 0 or "", because each of those is a claim about the recording that
#   the file never made. Where the input appeared to carry the field and it
#   could not be read, name the field in `unparsed` and warn once.
#
# `unparsed` is what separates case 3 from the other two. A field that is NA
# and is not named there has no value to be had. A field that is NA and is
# named there is a wording this package needs to learn, which is a thing worth
# reporting rather than a fact about the recording.
#
# The word is the one audioMetadata already uses: .mdUnparsed() names the
# chunks a file holds that nothing here can read, "so that a file is not taken
# to carry nothing just because nothing here can read what it carries". This is
# that idea at the level of a field rather than a chunk.
#
# An input that cannot be read at all stays an error rather than an absence, as
# it already is in .detectFormat() and .tsaParts(). There is no partial reading
# of such a file worth returning, and returning one would be this same fault in
# another place.

#' Warn that something was there to be read and was not read
#'
#' Classed, for the reason R/conditions.R gives for classing the warnings
#' raised while reading audio: a warning identified only by its wording cannot
#' be told from another except by matching text that is free to change. A walk
#' over a corpus that is expected to hold wordings this package does not know
#' can therefore silence this one kind without silencing a truncated recording
#' along with it, which is what makes warning here affordable at all.
#'
#' Every one of these is classed `unparsedField` as well as by what could not
#' be read, so that all of them can be handled together:
#'
#'   unparsedComment   An AudioMoth comment carries a clause this package
#'                     cannot read, so the fields that clause holds are not
#'                     reported.
#'   unparsedDatetime  A filename matched the shape of a format, and the digits
#'                     in it spell no real date or time of day.
#'   unparsedResults   A file in a directory of BirdNET results is in no layout
#'                     this package knows, so its detections were not read.
#'
#' The call is left out of the condition: these are raised from the helper that
#' detects the fault rather than from the function the caller called, so
#' reporting the call would name an internal helper.
#'
#' @param class The class naming what it is that could not be read
#' @param ... Parts of the message
#' @return Nothing, called for the condition it raises
#' @keywords internal
#' @noRd
.unparsedWarning <- function(class, ...) {
  warning(structure(
    class = c(class, "unparsedField", "warning", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' Name the fields an input appeared to carry and that were not read
#'
#' Each argument is named for a field and is TRUE where the input looks as
#' though it carries that field and nothing was read from it. A field that was
#' read, and a field the input says nothing about, are both left unnamed: the
#' first has an answer and the second has none to be had, and neither is
#' something for this package to learn.
#'
#' @param ... One named argument per field
#' @return A character vector of field names, or NULL where every field that
#'   was there was read
#' @keywords internal
#' @noRd
.unparsedFields <- function(...) {
  checks <- c(...)
  named <- names(checks)[!is.na(checks) & checks]
  if (length(named) == 0L) {
    return(NULL)
  }
  return(named)
}
