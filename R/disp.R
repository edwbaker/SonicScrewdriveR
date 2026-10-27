# The DISP chunk of a wave file: the name the file is shown under.
#
# RIFF's DISP chunk holds one clipboard item, introduced by the number of the
# clipboard format it is in. Written with a text format it is the title of the
# recording, and a sound library commonly writes that title here and nowhere
# else in the file.

#' The clipboard formats a DISP chunk can name
#'
#' The numbers Windows gives its predefined clipboard formats. Only the three
#' text ones carry anything this package can read; the rest are named so that a
#' caller can tell what the file is holding.
#' @keywords internal
#' @noRd
.DISP_FORMATS <- c(
  "1"="CF_TEXT", "2"="CF_BITMAP", "3"="CF_METAFILEPICT", "4"="CF_SYLK",
  "5"="CF_DIF", "6"="CF_TIFF", "7"="CF_OEMTEXT", "8"="CF_DIB",
  "9"="CF_PALETTE", "10"="CF_PENDATA", "11"="CF_RIFF", "12"="CF_WAVE",
  "13"="CF_UNICODETEXT", "14"="CF_ENHMETAFILE", "15"="CF_HDROP",
  "16"="CF_LOCALE", "17"="CF_DIBV5"
)

#' The clipboard formats that hold text, and the encoding each holds it in
#' @keywords internal
#' @noRd
.DISP_TEXT <- c("1"="latin1", "7"="latin1", "13"="UTF-16LE")

#' Read the display name of a wave file
#'
#' Reads the DISP chunks of a wave file. The chunk holds one clipboard item
#' and the number of the clipboard format it is in; with a text format that
#' item is the name the file is to be shown under, which for a recording out
#' of a sound library is commonly its title or the place it was made.
#'
#' A chunk holding no more than the format number, which is how most writers
#' that mention the chunk at all leave it, gives nothing: there is nothing in
#' it to report. A chunk holding something in a format that is not text gives
#' the name of that format and no value, so that a caller can tell the file
#' carries an image rather than a title.
#'
#' @param x Path to a wave file, a Wave object with DISP metadata attached, a
#'   raw vector holding a wave file, or a disp object
#' @return A disp object, which is a named list of fields: `format` naming the
#'   clipboard format, and `text` where that format was a text one. A file
#'   holding more than one DISP chunk gives `format.2` and `text.2` for the
#'   second of them, and so on.
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once.
#' @examples
#' \dontrun{
#' disp("recording.wav")
#' }
#'
disp <- function(x) {
  return(.mdDispatch(
    x, "disp",
    fromFile=function(file) .mdDispObject(.mdWavChunks(file, "DISP")),
    fromBytes=function(bytes) .mdDispObject(.mdWavChunksBytes(bytes, "DISP"))
  ))
}

#' Build a disp object from the bodies of the DISP chunks of a file
#' @keywords internal
#' @noRd
.mdDispObject <- function(bodies) {
  fields <- list()
  for (body in bodies) {
    # Four bytes for the format number, and whatever is in that format after
    # it. A chunk too short to hold the number says nothing about what it
    # holds, so there is nothing to report.
    if (length(body) < 4L) {
      next
    }
    item <- body[-seq_len(4L)]
    if (length(item) == 0L) {
      next
    }
    # Indexing by name with a single bracket gives NA for a format number
    # Windows does not define, where a double bracket would raise an error.
    code <- as.character(.rawToUInt(body[seq_len(4L)]))
    named <- .DISP_FORMATS[code]
    fields[[.mdUniqueName(fields, "format")]] <-
      if (is.na(named)) code else unname(named)
    encoding <- .DISP_TEXT[code]
    if (is.na(encoding)) {
      next
    }
    text <- trimws(.mdDecode(item, from=unname(encoding)))
    if (nzchar(text)) {
      fields[[.mdUniqueName(fields, "text")]] <- text
    }
  }
  return(.mdObject(fields, "disp"))
}
