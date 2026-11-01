#' Read AudioMoth configuration file
#'
#' Reads and parses an AudioMoth configuration file.
#'
#' @param filename Path to the configuration file to read
#' @return A data frame of the configuration settings, with columns Key and Value
#' @export
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' audiomothConfig("./CONFIG.TXT")
#' }
#'
audiomothConfig <- function(filename) {
  f <- readLines(filename)
  c <- read.csv(textConnection(sub(":", "|", f)), header=FALSE, sep="|")
  #Both columns. Replacing the colon with a separator leaves a space at the front
  #of every value, so comparing a value against a string never matched.
  c[] <- lapply(c, trimws)
  colnames(c) <- c("Key", "Value")
  return(c)
}

#' Capture groups from an AudioMoth comment
#'
#' @param x The comment string to match against
#' @param pattern A regular expression containing at least one capture group
#' @return The capture groups of the first match, or NULL if there is no match
#' @noRd
audiomothMatch <- function(x, pattern) {
  m <- regmatches(x, regexec(pattern, x, perl=TRUE))[[1]]
  if (length(m) < 2) {
    return(NULL)
  }
  return(m[-1])
}

#' A single field from an AudioMoth comment
#'
#' @param x The comment string to match against
#' @param pattern A regular expression containing at least one capture group
#' @return The first capture group of the first match, or NA if there is no match
#' @noRd
audiomothField <- function(x, pattern) {
  m <- audiomothMatch(x, pattern)
  if (is.null(m)) {
    return(NA_character_)
  }
  return(m[1])
}

#' Read AudioMoth metadata from a wave file
#'
#' Deprecated. Reads and parses metadata stored in wave files produced by
#' AudioMoth devices.
#'
#' Deprecated in favour of [readMetadata()], which reads the AudioMoth comment
#' along with every other kind of metadata a file carries and returns these
#' same fields as its `sources$audiomoth`. It also normalises the recording
#' time into a `datetime`, which this function leaves as the separate
#' `start_date`, `start_time` and `time_zone` strings the recorder wrote.
#'
#' The comment written into the wave header has been reworded several times
#' across AudioMoth-Firmware-Basic releases, most significantly in 1.6.0, where
#' "at medium gain setting while battery state was" became "at medium gain while
#' battery was" and the filter and cancellation clauses were rephrased. Fields
#' are matched by pattern rather than by position, so all wordings from 1.0
#' onwards are read.
#'
#' Firmware from 1.5.0 can be configured with a deployment ID, which the
#' firmware writes in place of the device serial number. Such files have a
#' \code{deployment_id} and no \code{serial}.
#'
#' Firmware before 1.4.0 wrote a gain index rather than a gain name, so
#' \code{gain} is a number ("0" to "4") for files from those versions.
#'
#' @param filename Path to the wave file to read
#' @return A list of extracted parameters:
#' \describe{
#'   \item{raw}{The comment string the other fields were read from}
#'   \item{start_time}{Recording start time, as \code{"HH:MM:SS"}}
#'   \item{start_date}{Recording start date, as \code{"DD/MM/YYYY"}}
#'   \item{time_zone}{Time zone of the start time, e.g. \code{"UTC"} or
#'     \code{"UTC+1"}}
#'   \item{serial}{The device serial number, or NA for a deployment ID file}
#'   \item{deployment_id}{The configured deployment ID, or NA if none was set}
#'   \item{gain}{Gain setting, one of "low", "low-medium", "medium",
#'     "medium-high" or "high"}
#'   \item{voltage}{Battery voltage in volts. Where the firmware reported only
#'     that the battery was outside the range it measures, the threshold it
#'     compared against is returned with a leading \code{"<"} or \code{">"},
#'     e.g. \code{"<2.5"}. The thresholds themselves have changed: firmware up
#'     to 1.2.0 wrote \code{"<3.6"} and \code{">5.0"}, 1.2.2 and 1.3.0
#'     \code{"<3.6"} and \code{">4.9"}, and 1.4.0 onwards \code{"<2.5"} and
#'     \code{">4.9"}}
#'   \item{temperature}{Temperature in degrees Celsius, or NA for firmware
#'     before 1.4.0, which did not record it}
#'   \item{external_mic}{Whether an external microphone was used}
#'   \item{filter}{"Low-pass", "Band-pass", "High-pass", or FALSE if no filter
#'     was applied}
#'   \item{filter.limit}{Filter frequency as the recorder writes it in its
#'     comment: text, in kHz, such as \code{"5.0"}, or two values separated by
#'     a hyphen for a band-pass filter, such as \code{"1.0-5.0"}. It is not
#'     converted to Hz, the unit of the frequencies the package measures,
#'     as it is a reading of the recorder's own record. FALSE if no filter was
#'     applied}
#'   \item{cancelled}{Why the recording stopped early, or FALSE if it ran to
#'     completion}
#'   \item{unparsed}{The fields the comment appeared to carry and that could
#'     not be read, where there are any. Absent otherwise}
#' }
#'
#' @section Fields that were not read:
#' A field this cannot read is NA, and is named in `unparsed`. Nothing is
#' reported as a default: `filter`, `filter.limit`, `cancelled` and
#' `external_mic` are FALSE only where the comment carries no clause of that
#' kind at all, which is how the firmware says that no filter was applied, that
#' the recording ran to completion and that the built-in microphone was used —
#' by writing nothing. Where it carries such a clause in a wording this package
#' does not know, the field is NA rather than FALSE, since FALSE would be a
#' claim about the recording that the file never made.
#'
#' `unparsed` is therefore what tells the two apart: a field that is NA and is
#' not named there has no value to be had, and one that is NA and is named
#' there is a wording this package needs to learn. A warning classed
#' `unparsedComment`, and `unparsedField`, carries the comment so that it can
#' be reported; class it to silence this one kind of warning while reading a
#' corpus that is known to hold wordings this package does not know.
#'
#' A comment cut short names nothing in `unparsed` and warns about nothing.
#' Every complete comment ends in a full stop, so one that does not was cut —
#' a version 1 ID3 tag allows a comment 30 bytes, which the recorder's own
#' wording fills before it reaches the end of the date — and what is past the
#' cut is absent from the file rather than unread. There is no wording to
#' report in that case, only a shorter comment than the recorder wrote.
#' @seealso [readMetadata()], which reads this alongside every other kind of
#'   metadata a file holds, and whose `sources$audiomoth` is what this function
#'   returns.
#' @export
#' @examples
#' \dontrun{
#' # Deprecated. Prefer:
#' readMetadata("./FILENAME.WAV")$sources$audiomoth
#' }
#'
audiomothWave <- function(filename) {
  .Deprecated(
    "readMetadata",
    package="sonicscrewdriver",
    msg=paste(
      "audiomothWave() is deprecated and will be removed in a future release.",
      "Use readMetadata(), whose $sources$audiomoth holds the same fields."
    )
  )
  return(.audiomothComment(.audiomothWaveComment(filename)))
}

#' The AudioMoth comment a wave file holds, if it holds one
#'
#' The recorder writes its comment into a `LIST`/`INFO` `ICMT` chunk, and a
#' writer that has since touched the file may have copied it into an ID3
#' `COMM` frame as well. Either will do, so both are offered to the parser.
#'
#' Reading the chunk is what makes the order of the chunks irrelevant. Scanning
#' the start of the file instead, as this once did, found the comment only
#' where the recorder had written `LIST` ahead of `data`; a file whose `LIST`
#' trails its audio read as though it held no AudioMoth comment at all.
#'
#' @param x A path, raw bytes, or a chunk map
#' @return A character vector of candidate comments, empty where there are none
#' @keywords internal
#' @noRd
.audiomothWaveComment <- function(x) {
  # A file that is not there is the caller's mistake and is worth saying so,
  # where a file that is there but holds no list is simply a file with no
  # AudioMoth comment in it.
  if (is.character(x)) {
    .mdCheckFile(x)
  }
  out <- character(0)
  info <- tryCatch(wavInfo(x), error=function(cond) NULL)
  if (!is.null(info)) {
    out <- c(out, .mdPick(info, "ICMT"))
  }
  tag <- tryCatch(id3(x), error=function(cond) NULL)
  if (!is.null(tag)) {
    out <- c(out, .mdPickPrefix(tag, "COMM"))
  }
  return(out[!is.na(out)])
}

#' Read the fields an AudioMoth comment carries
#'
#' The parsing itself, kept apart from where the comment came from, so that a
#' caller already holding one — [readMetadata()] has read it out of the chunk
#' list before it asks — does not send the reader back to the file for it.
#'
#' @param x One or more candidate comment strings
#' @return The fields [audiomothWave()] documents, or an empty list where no
#'   string carries an AudioMoth comment
#' @keywords internal
#' @noRd
.audiomothComment <- function(x) {
  raw <- x[grep("Recorded", x, useBytes = TRUE)]

  if (length(raw)==0) {
    return(list())
  }
  raw <- raw[1]

  start_time <- audiomothField(raw, "([0-2][0-9]:[0-6][0-9]:[0-6][0-9])")
  start_date <- audiomothField(raw, "([0-3][0-9]/[0-1][0-9]/[0-9]{4})")
  time_zone <- audiomothField(raw, "\\((UTC[^)]*)\\)")

  # The firmware writes either the device serial number ("by AudioMoth <id>")
  # or, where a deployment ID has been configured, that ID in its place
  # ("during deployment <id>"). Only one of the two is ever present.
  serial <- audiomothField(raw, "by AudioMoth ([0-9A-F]{16})")
  deployment_id <- audiomothField(raw, "during deployment ([0-9A-F]{16})")

  # Up to 1.5.0 the gain was followed by "gain setting while battery state
  # was", from 1.6.0 by "gain while battery was". Before 1.4.0 the gain was
  # written as an index instead of a name.
  gain <- audiomothField(raw, "at (low-medium|medium-high|low|medium|high) gain")
  if (is.na(gain)) {
    gain <- audiomothField(raw, "at gain setting ([0-9]+) while")
  }

  # Where the battery was outside the range the firmware measures, the state is
  # written as a comparison against a threshold, and both the wording and the
  # threshold have changed: 1.0 to 1.2.0 wrote "< 3.6V" and "> 5.0V", 1.2.2 and
  # 1.3.0 "less than 3.6V" and "greater than 4.9V", and 1.4.0 onwards "less
  # than 2.5V" and "greater than 4.9V". Reading the comparator separately from
  # the threshold keeps every one of them. Firmware 1.0 wrote the measured
  # voltage with two decimal places, the rest with one.
  m <- audiomothMatch(
    raw,
    "battery(?: state)? was (less than|greater than|<|>) ?([0-9]+\\.[0-9]{1,2})V"
  )
  if (!is.null(m)) {
    voltage <- paste0(
      if (is.element(m[1], c("less than", "<"))) "<" else ">",
      m[2]
    )
  } else {
    voltage <- audiomothField(raw, "battery(?: state)? was ([0-9]+\\.[0-9]{1,2})V")
  }

  temp <- audiomothField(raw, "temperature was ([-+]?[0-9]+\\.[0-9]+)C")

  external_mic <- grepl("using external microphone", raw)

  # Up to 1.5.0: "Low-pass filter applied with cut-off frequency of 5.0kHz.",
  # from 1.6.0: "Low-pass filter with frequency of 5.0kHz applied." Reading the
  # frequency out of the filter clause itself keeps it from picking up the
  # frequency trigger, which firmware from 1.7.0 writes earlier in the comment
  # and which also carries a value in kHz.
  filter <- FALSE
  filter_limit <- FALSE
  m <- audiomothMatch(
    raw,
    "(Low|High)-pass filter (?:applied )?with (?:cut-off )?frequency of ([0-9]+\\.[0-9]+)kHz"
  )
  if (!is.null(m)) {
    filter <- paste0(m[1], "-pass")
    filter_limit <- m[2]
  }
  m <- audiomothMatch(
    raw,
    paste0(
      "Band-pass filter (?:applied )?with (?:cut-off )?frequencies of ",
      "([0-9]+\\.[0-9]+)kHz and ([0-9]+\\.[0-9]+)kHz"
    )
  )
  if (!is.null(m)) {
    filter <- "Band-pass"
    filter_limit <- paste0(m[1], "-", m[2])
  }

  # Up to 1.5.0 the clause read "Recording cancelled before completion due to
  # <reason>", from 1.6.0 "Recording stopped due to <reason>" or, for the
  # magnetic switch, "Recording stopped by <reason>". The switch reason was
  # reworded in 1.6.0 and is reported under its original name.
  cancelled <- FALSE
  reason <- audiomothField(
    raw,
    "Recording (?:cancelled before completion|stopped) (?:due to|by) ([^.]+)\\."
  )
  if (!is.na(reason)) {
    if (reason == "switch position change") {
      reason <- "change of switch position"
    }
    cancelled <- reason
  }

  # Which fields the comment looks as though it carries and nothing was read
  # from. Each test is a word that every wording of that clause has held, so a
  # rewording is caught without knowing what the new wording is, which is the
  # only way it can be caught before the release that writes it.
  #
  # The words are chosen to be the clause's alone. "external" rather than
  # "microphone", because a recording stopped by a microphone change says
  # "microphone" in a clause that has nothing to do with an external one.
  # "battery" rather than "voltage", because a stop due to low voltage says
  # "voltage". "Recording" belongs to no clause but the one reporting an early
  # stop, the comment itself opening "Recorded at".
  has <- function(what) grepl(what, raw, ignore.case=TRUE)

  # A comment cut short carries nothing past the cut, so what is missing from
  # it is missing rather than unread, and there is nothing for this package to
  # learn from it. Every complete comment ends in a full stop, whichever clause
  # it ends with, so one that does not was cut: a version 1 ID3 tag allows a
  # comment 30 bytes, which the recorder's own wording fills before it reaches
  # the end of the date.
  whole <- grepl("[.][[:space:]]*$", raw)

  unparsed <- if (!whole) NULL else .unparsedFields(
    # The clauses every firmware writes unconditionally: a start time, a date,
    # a device identifier of one of the two kinds, a gain and a battery state.
    # There is no complete AudioMoth comment without them, so one of them
    # unread needs no keyword to tell it is a wording this has not seen — being
    # NA is the whole of the evidence.
    start_time = is.na(start_time),
    start_date = is.na(start_date),
    serial = is.na(serial) && is.na(deployment_id),
    # The zone is gated, although every firmware from 1.2.0 writes it: the
    # comments this package records for the releases before that carry no zone
    # at all, and a comment without one is not a comment whose zone went
    # unread. Where it names a zone and the zone did not read, the way it is
    # written has changed, which is worth reporting — #75 was an offset hour
    # of one digit doing exactly that in a GUANO timestamp.
    time_zone = grepl("UTC", raw, fixed=TRUE) && is.na(time_zone),
    gain = is.na(gain),
    voltage = is.na(voltage),
    # Firmware before 1.4.0 wrote no temperature at all, so this one is gated
    # on the comment mentioning one: a comment that does not is not a comment
    # whose temperature went unread.
    temperature = has("temperature") && is.na(temp),
    external_mic = has("external") && !external_mic,
    filter = has("filter") && isFALSE(filter),
    filter.limit = has("filter") && isFALSE(filter_limit),
    cancelled = has("Recording") && isFALSE(cancelled)
  )

  # A clause that was not read reports nothing, where a default left standing
  # would be a positive claim about the recording. FALSE is exactly that for
  # these three: that no filter was applied, that no external microphone was
  # used, and that the recording ran to completion. It is the right reading
  # where the comment carries no such clause, which is how the firmware says
  # all three — by writing nothing — and the wrong one where it carries a
  # clause this could not read.
  if (is.element("external_mic", unparsed)) {
    external_mic <- NA
  }
  if (is.element("filter", unparsed)) {
    filter <- NA
  }
  if (is.element("filter.limit", unparsed)) {
    filter_limit <- NA
  }
  if (is.element("cancelled", unparsed)) {
    cancelled <- NA
  }

  ret = list(
    "raw" = raw,
    "start_time" = start_time,
    "start_date" = start_date,
    "time_zone" = time_zone,
    "serial" = serial,
    "deployment_id" = deployment_id,
    "gain" = gain,
    "voltage" = voltage,
    "temperature" = temp,
    "external_mic" = external_mic,
    "filter" = filter,
    "filter.limit" = filter_limit,
    "cancelled" = cancelled
  )
  if (!is.null(unparsed)) {
    # Reported only where there is something to report, as the fields of a
    # reading are throughout this package, so that a comment every field of
    # which was read gives what it always gave.
    ret$unparsed <- unparsed
    .unparsedWarning(
      "unparsedComment",
      "This AudioMoth comment carries ", paste(unparsed, collapse=", "),
      " in a wording this package cannot read, so ",
      if (length(unparsed) == 1L) "it is" else "they are",
      " not reported. Please report the wording, which a firmware release",
      " this package has not seen writes: '", raw, "'"
    )
  }
  return(ret)
}
