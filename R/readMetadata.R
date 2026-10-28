# One reading of every kind of metadata a file carries.
#
# A recording can carry its record in any of half a dozen conventions, and
# which one depends on the device that made it and on every piece of software
# that has handled it since. A file commonly carries more than one, and the
# same fact under a different name in each. readMetadata() reads all of them
# and, beside them, one normalised record built from whichever of them said
# each thing.

#' The order the normalised fields are reported in
#' @keywords internal
#' @noRd
.MD_FIELD_ORDER <- c(
  "datetime", "make", "model", "serial", "firmware", "software", "prefix",
  "microphone", "title", "species", "recordist", "comment", "note", "licence",
  "url", "location", "elevation", "collection", "project", "take"
)

#' The order the sources are read in, which is the order they are trusted in
#'
#' A format written for bioacoustics is trusted over one written for broadcast,
#' and anything written inside the file over anything read from its name: a
#' name is a convention that a rename or a download can break, and the readings
#' of a name that is only digits are ambiguous.
#' @keywords internal
#' @noRd
.MD_SOURCE_ORDER <- c(
  "guano", "wamd", "audiomoth", "bext", "ixml", "wavInfo", "xmp", "disp",
  "id3", "vorbisComment", "filename"
)

#' Pick the first of several fields that holds something
#' @keywords internal
#' @noRd
.mdPick <- function(fields, ...) {
  for (name in c(...)) {
    value <- fields[[name]]
    if (!is.null(value) && length(value) > 0L &&
        (!is.character(value) || any(nzchar(value)))) {
      return(value)
    }
  }
  return(NULL)
}

#' Pick the first field whose name begins with a given string
#'
#' A comment frame is named by its description as well as by its identifier, so
#' the name it ends up under is not known in advance.
#' @keywords internal
#' @noRd
.mdPickPrefix <- function(fields, prefix) {
  for (at in grep(paste0("^", prefix), names(fields))) {
    value <- fields[[at]]
    if (!is.null(value) && length(value) > 0L &&
        (!is.character(value) || any(nzchar(value)))) {
      return(value)
    }
  }
  return(NULL)
}

#' Read a date and time written as text
#'
#' Only a full date gives a time. A field holding no more than a year is left
#' where it was read rather than being given a month and a day it never had.
#' The date and the time may be separated by a T or by a space, the time may
#' stop at the minute, as ID3v2.4 and XMP allow, and an offset from UTC may be
#' written as +02:00, +0200 or +2:00. A value that only begins with a date, or
#' with a date and a time, is not read.
#'
#' @param text The field as it is written
#' @return A POSIXct, or NULL where it is not a date this understands
#' @keywords internal
#' @noRd
.mdDate <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }
  if (inherits(text, "POSIXt")) {
    return(text)
  }
  text <- trimws(as.character(text)[1])
  if (is.na(text) || !nzchar(text)) {
    return(NULL)
  }
  wanted <- sub("^([0-9]{4}-[0-9]{1,2}-[0-9]{1,2})[[:space:]]+", "\\1T", text)
  # An offset hour of one digit, as +2:00, which the Song Meter Mini Bat writes
  # in its GUANO, is given its second digit, so that the offset is read rather
  # than taken for no offset at all.
  wanted <- sub("([+-])([0-9]):([0-9]{2})$", "\\10\\2:\\3", wanted)
  # R does not accept the colon that ISO 8601 puts in a numeric UTC offset.
  wanted <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", wanted)
  # The shape of each form is checked before it is read, as as.POSIXct() is
  # content with a value that merely begins with one: without this, a time
  # followed by anything it did not recognise, an offset included, would be read
  # as a wall clock and the rest quietly dropped, and a date followed by a time
  # it could not read would be read as midnight. R reads a field of one digit
  # as well as one of two, and an offset with spaces before it, so those are
  # allowed.
  day <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}"
  minute <- paste0(day, "T[0-9]{1,2}:[0-9]{1,2}")
  clock <- paste0(minute, ":[0-9]{1,2}([.][0-9]+)?")
  forms <- list(
    list(shape=paste0(clock, " *[+-][0-9]{4}$"),
         format="%Y-%m-%dT%H:%M:%OS%z", tz="UTC"),
    list(shape=paste0(clock, "Z$"),
         format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
    list(shape=paste0(clock, "$"),
         format="%Y-%m-%dT%H:%M:%OS", tz=""),
    list(shape=paste0(minute, " *[+-][0-9]{4}$"),
         format="%Y-%m-%dT%H:%M%z", tz="UTC"),
    list(shape=paste0(minute, "Z$"),
         format="%Y-%m-%dT%H:%MZ", tz="UTC"),
    list(shape=paste0(minute, "$"),
         format="%Y-%m-%dT%H:%M", tz=""),
    list(shape=paste0(day, "$"),
         format="%Y-%m-%d", tz="")
  )
  for (form in forms) {
    if (!grepl(form$shape, wanted)) {
      next
    }
    parsed <- suppressWarnings(as.POSIXct(wanted, format=form$format, tz=form$tz))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  return(NULL)
}

#' Read the time an AudioMoth comment records
#'
#' The comment gives a wall clock and, in brackets after it, the offset from
#' UTC the device was set to.
#'
#' @param a The fields audiomothWave() read
#' @return A POSIXct, or NULL where the comment held no time
#' @keywords internal
#' @noRd
.mdAudiomothDatetime <- function(a) {
  date <- a$start_date
  time <- a$start_time
  if (length(date) != 1L || is.na(date) || length(time) != 1L || is.na(time)) {
    return(NULL)
  }
  clock <- suppressWarnings(as.POSIXct(
    paste(date, time), format="%d/%m/%Y %H:%M:%S", tz="UTC"
  ))
  if (is.na(clock)) {
    return(NULL)
  }
  zone <- a$time_zone
  if (length(zone) != 1L || is.na(zone) || !grepl("^UTC[+-]", zone)) {
    return(clock)
  }
  hours <- suppressWarnings(as.numeric(sub("^UTC[+-]([0-9]+).*$", "\\1", zone)))
  minutes <- if (grepl("^UTC[+-][0-9]+:[0-9]+", zone)) {
    suppressWarnings(as.numeric(sub("^UTC[+-][0-9]+:([0-9]+).*$", "\\1", zone)))
  } else {
    0
  }
  if (is.na(hours) || is.na(minutes)) {
    return(clock)
  }
  offset <- (hours * 3600 + minutes * 60) * if (grepl("^UTC-", zone)) -1 else 1
  # The clock was read as though it were UTC, so the offset the device was set
  # to has to come back off it to give the moment the recording was made.
  return(clock - offset)
}

#' The normalised fields GUANO metadata gives
#' @keywords internal
#' @noRd
.mdNormaliseGuano <- function(g) {
  position <- .mdPick(g, "Loc Position")
  return(list(
    datetime = .mdDate(.mdPick(g, "Timestamp")),
    make = .mdPick(g, "Make"),
    model = .mdPick(g, "Model"),
    serial = .mdPick(g, "Serial"),
    firmware = .mdPick(g, "Firmware Version"),
    species = .mdPick(g, "Species Manual ID", "Species Auto ID"),
    note = .mdPick(g, "Note"),
    location = if (is.numeric(position) && length(position) >= 2L) {
      c(latitude=unname(position[1]), longitude=unname(position[2]))
    } else {
      NULL
    },
    elevation = .mdPick(g, "Loc Elevation")
  ))
}

#' The normalised fields Wildlife Acoustics metadata gives
#' @keywords internal
#' @noRd
.mdNormaliseWamd <- function(w) {
  position <- .mdPick(w, "gpsfirst")
  numeric <- is.numeric(position) && length(position) >= 2L
  return(list(
    datetime = .mdDate(.mdPick(w, "timestamp")),
    # The chunk is Wildlife Acoustics' own format, so a file carrying one was
    # written by one of their devices whether or not it says so.
    make = "Wildlife Acoustics",
    model = .mdPick(w, "model"),
    serial = .mdPick(w, "serial"),
    firmware = .mdPick(w, "firmware"),
    software = .mdPick(w, "software"),
    prefix = .mdPick(w, "prefix"),
    microphone = .mdPick(w, "microphone"),
    species = .mdPick(w, "manual_id", "auto_id"),
    note = .mdPick(w, "notes"),
    licence = .mdPick(w, "license"),
    location = if (numeric) {
      c(latitude=unname(position[1]), longitude=unname(position[2]))
    } else {
      NULL
    },
    elevation = if (numeric && !is.na(position["altitude"])) {
      unname(position["altitude"])
    } else {
      NULL
    }
  ))
}

#' The normalised fields an AudioMoth comment gives
#' @keywords internal
#' @noRd
.mdNormaliseAudiomoth <- function(a) {
  return(list(
    datetime = .mdAudiomothDatetime(a),
    make = "Open Acoustic Devices",
    model = "AudioMoth",
    serial = if (length(a$serial) == 1L && !is.na(a$serial)) a$serial else NULL,
    comment = if (length(a$raw) == 1L && !is.na(a$raw)) a$raw else NULL
  ))
}

#' The normalised fields Broadcast Wave metadata gives
#' @keywords internal
#' @noRd
.mdNormaliseBext <- function(b) {
  return(list(
    datetime = .mdPick(b, "datetime"),
    # A sound library commonly writes the whole of its record for a recording
    # into the description, which is the nearest thing the chunk has to a
    # title of its own.
    title = .mdPick(b, "Description"),
    recordist = .mdPick(b, "Originator")
  ))
}

#' The normalised fields iXML metadata gives
#' @keywords internal
#' @noRd
.mdNormaliseIxml <- function(i) {
  return(list(
    project = .mdPick(i, "PROJECT"),
    take = .mdPick(i, "TAKE"),
    note = .mdPick(i, "NOTE")
  ))
}

#' The normalised fields an INFO list gives
#' @keywords internal
#' @noRd
.mdNormaliseInfo <- function(i) {
  return(list(
    datetime = .mdDate(.mdPick(i, "ICRD")),
    title = .mdPick(i, "INAM"),
    recordist = .mdPick(i, "IART"),
    comment = .mdPick(i, "ICMT"),
    software = .mdPick(i, "ISFT"),
    licence = .mdPick(i, "ICOP")
  ))
}

#' The normalised fields an XMP packet gives
#'
#' The audio properties the packet holds describe the recording as the editor
#' read it rather than as the file declares it, so they are left in the packet
#' and not reported as the record of the file. What the editor was told about
#' the recording is reported.
#' @keywords internal
#' @noRd
.mdNormaliseXmp <- function(x) {
  return(list(
    datetime = .mdDate(.mdPick(x, "xmp:CreateDate")),
    title = .mdPick(x, "dc:title", "xmpDM:trackName"),
    recordist = .mdPick(x, "xmpDM:artist", "dc:creator"),
    comment = .mdPick(x, "dc:description", "xmpDM:logComment"),
    licence = .mdPick(x, "dc:rights"),
    collection = .mdPick(x, "xmpDM:album")
  ))
}

#' The normalised fields a display name gives
#' @keywords internal
#' @noRd
.mdNormaliseDisp <- function(d) {
  return(list(
    title = .mdPick(d, "text")
  ))
}

#' The normalised fields an ID3 tag gives
#' @keywords internal
#' @noRd
.mdNormaliseId3 <- function(i) {
  return(list(
    datetime = .mdDate(.mdPick(i, "TDRC", "TDRL", "TDOR")),
    # The three-character identifiers are those of version 2.2, and the
    # lowercase names those of a version 1 tag.
    title = .mdPick(i, "TIT2", "TT2", "title"),
    recordist = .mdPick(i, "TPE1", "TP1", "artist"),
    comment = .mdPickPrefix(i, "(comment|COMM)"),
    licence = .mdPick(i, "WCOP", "WCP"),
    url = .mdPick(i, "WOAF", "WOA", "WOAR", "WOR")
  ))
}

#' The normalised fields Vorbis comments give
#' @keywords internal
#' @noRd
.mdNormaliseVorbis <- function(v) {
  return(list(
    datetime = .mdDate(.mdPick(v, "DATE")),
    title = .mdPick(v, "TITLE"),
    recordist = .mdPick(v, "ARTIST", "PERFORMER"),
    comment = .mdPick(v, "COMMENT", "DESCRIPTION"),
    species = .mdPick(v, "SPECIES"),
    licence = .mdPick(v, "LICENSE", "COPYRIGHT"),
    url = .mdPick(v, "CONTACT"),
    software = .mdPick(v, "ENCODER", "vendor")
  ))
}

#' The normalised fields a filename gives
#' @keywords internal
#' @noRd
.mdNormaliseFilename <- function(p) {
  one <- function(value) {
    if (length(value) == 1L && !is.na(value)) value else NULL
  }
  return(list(
    # A name of eight or ten digits is read as a date or as a count of seconds,
    # and one that spells neither a real date nor a real time of day is read as
    # a time that is missing. Reporting that would put a field in the record
    # saying the recording has a date and that the date is not known, which is
    # a worse answer than the record simply not mentioning one.
    #
    # A reading the name itself left in doubt is not reported either. A
    # collection that numbers its records gives names of the same shape as a
    # date or a count of seconds, and this is the one source that nothing
    # outranks: where the file said nothing, a guess from its name would be
    # the whole answer, and a confident wrong date is worse than none.
    datetime = if (isTRUE(p$ambiguous)) {
      NULL
    } else if (inherits(p$datetime, "POSIXt") && length(p$datetime) == 1L &&
               !is.na(p$datetime)) {
      p$datetime
    } else {
      NULL
    },
    model = one(p$model),
    prefix = one(p$prefix),
    species = one(p$taxon),
    collection = one(p$collection)
  ))
}

#' Merge the normalised fields of several sources
#'
#' The sources are given in the order they are trusted, and the first of them
#' to say a thing is the one that is believed.
#'
#' @param sets The normalised fields of each source, most trusted first
#' @return One named list, in the order the fields are reported in
#' @keywords internal
#' @noRd
.mdMergeFields <- function(sets) {
  fields <- list()
  for (set in sets) {
    for (name in names(set)) {
      value <- set[[name]]
      if (is.null(fields[[name]]) && !is.null(value) && length(value) > 0L &&
          (!is.character(value) || any(nzchar(value)))) {
        fields[[name]] <- value
      }
    }
  }
  known <- .MD_FIELD_ORDER[.MD_FIELD_ORDER %in% names(fields)]
  return(fields[c(known, setdiff(names(fields), known))])
}

#' Normalise the fields of one source
#' @keywords internal
#' @noRd
.mdNormalise <- function(name, source) {
  return(switch(
    name,
    guano = .mdNormaliseGuano(source),
    wamd = .mdNormaliseWamd(source),
    audiomoth = .mdNormaliseAudiomoth(source),
    bext = .mdNormaliseBext(source),
    ixml = .mdNormaliseIxml(source),
    wavInfo = .mdNormaliseInfo(source),
    xmp = .mdNormaliseXmp(source),
    disp = .mdNormaliseDisp(source),
    id3 = .mdNormaliseId3(source),
    vorbisComment = .mdNormaliseVorbis(source),
    filename = .mdNormaliseFilename(source),
    list()
  ))
}

#' Does a source hold anything?
#' @keywords internal
#' @noRd
.mdHasFields <- function(source) {
  return(!is.null(source) && length(source) > 0L)
}

#' Read the AudioMoth comment of a wave file, if it has one
#'
#' The comment is read by [audiomothWave()], which takes a path, so this is
#' only read from a file and not from bytes already in hand.
#' @keywords internal
#' @noRd
.mdAudiomothSource <- function(file, info) {
  comment <- .mdPick(info, "ICMT")
  if (is.null(comment) || !any(grepl("Recorded at", comment, fixed=TRUE))) {
    return(NULL)
  }
  read <- tryCatch(audiomothWave(file), error=function(cond) NULL)
  if (!.mdHasFields(read)) {
    return(NULL)
  }
  return(read)
}

#' Read what a filename says, where it says anything
#'
#' [parseFilename()] raises an error for a name that matches none of the
#' conventions it knows, which most names do, so a failure to read one is not
#' an error here.
#' @keywords internal
#' @noRd
.mdFilenameSource <- function(file, format=NULL) {
  read <- tryCatch(
    parseFilename(file, format=format),
    error=function(cond) NULL,
    warning=function(cond) NULL
  )
  if (!.mdHasFields(read)) {
    return(NULL)
  }
  return(read)
}

#' Which filename format to read, from what the caller asked for
#'
#' `filename` says whether to read the name at all, and may instead name the
#' convention the caller knows their files follow. Naming it settles what the
#' name means, so the date it holds is used rather than passed over as a guess.
#'
#' The name is checked here rather than where it is used, so that a format this
#' package does not know is an error at once instead of a file quietly reading
#' as though nothing had been asked for.
#'
#' @param filename What the caller passed
#' @return NULL to read no name, NA to detect the format, or the format named
#' @keywords internal
#' @noRd
.mdFilenameFormat <- function(filename) {
  if (isTRUE(filename)) {
    return(NA_character_)
  }
  if (isFALSE(filename)) {
    return(NULL)
  }
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("filename must be TRUE, FALSE, or the name of a filename format.")
  }
  # "match" picks a format that fits a list of filenames, and there is one file
  # here, so there is nothing for it to match against.
  if (!filename %in% .knownFileFormats()) {
    stop(
      "Unknown filename format: ", filename,
      ". One of: ", paste(.knownFileFormats(), collapse=", "), "."
    )
  }
  return(filename)
}

#' Name the chunks a file holds that carry metadata this package cannot read
#'
#' Reported so that a file is not taken to carry nothing just because nothing
#' here can read what it carries. The proprietary blocks that editing software
#' leaves behind and the loop and sample descriptions a sampler writes are all
#' metadata, and knowing they are there is worth more than passing over them in
#' silence.
#'
#' @param ids The chunk identifiers the file holds
#' @return A character vector of identifiers
#' @keywords internal
#' @noRd
.mdUnparsed <- function(ids) {
  # The chunks read above, and those that hold no metadata to read: the audio
  # itself, the sizes that describe it, the padding a writer leaves to make
  # room for metadata it may add later, and the cue points.
  known <- c("fmt ", "data", "fact", "ds64", "guan", "wamd", "bext", "iXML",
             "LIST", "_PMX", "DISP", "id3 ", "ID3 ", "PAD ", "JUNK", "junk",
             "cue ")
  return(unique(ids[!ids %in% known]))
}

#' Build an audioMetadata object
#' @keywords internal
#' @noRd
.mdObjectAll <- function(file, container, sources, unparsed) {
  sources <- sources[vapply(sources, .mdHasFields, logical(1))]
  order <- .MD_SOURCE_ORDER[.MD_SOURCE_ORDER %in% names(sources)]
  sources <- sources[c(order, setdiff(names(sources), order))]
  fields <- .mdMergeFields(lapply(
    names(sources),
    function(name) .mdNormalise(name, sources[[name]])
  ))
  return(structure(
    list(
      file = file,
      container = container,
      fields = fields,
      sources = sources,
      unparsed = unparsed
    ),
    class = "audioMetadata"
  ))
}

#' Read every kind of metadata a file carries
#'
#' Reads the embedded metadata of an audio file, whatever format that metadata
#' is in and whatever container the file is: GUANO, Wildlife Acoustics wamd,
#' Broadcast Wave bext, iXML and the INFO list from a wave file, the AudioMoth
#' comment, ID3 from an MP3 file or from a wave file that holds a tag in a
#' chunk, and Vorbis comments from a FLAC or Ogg file. A file holding none of
#' them gives an empty result rather than an error. A wave file over 4 GiB,
#' written in the RF64 or BW64 form, is read as any other, and named by that
#' form.
#'
#' @details
#' A recording commonly carries more than one of these formats, written by the
#' device that made it and by whatever software has handled it since, and the
#' same fact under a different name in each. The result therefore holds both
#' what each format said and one normalised record built from all of them:
#'
#' \describe{
#'   \item{file}{The path the metadata was read from, or NA}
#'   \item{container}{The kind of file it was read from, read from the bytes of
#'     the file rather than from its name, or NA where it is not one this
#'     package recognises}
#'   \item{fields}{The normalised record (see below)}
#'   \item{sources}{The formats that were found, each as the object its own
#'     reader gives, named `guano`, `wamd`, `audiomoth`, `bext`, `ixml`,
#'     `wavInfo`, `xmp`, `disp`, `id3`, `vorbisComment` and `filename`. A
#'     format the file does not carry is left out.}
#'   \item{unparsed}{The identifiers of any chunks the file holds that carry
#'     metadata this package cannot read, such as the proprietary block an
#'     editor leaves behind. Knowing they are there is worth more than passing
#'     over them in silence.}
#' }
#'
#' ## The normalised record
#' `fields` holds whichever of `datetime`, `make`, `model`, `serial`,
#' `firmware`, `software`, `prefix`, `microphone`, `title`, `species`,
#' `recordist`, `comment`, `note`, `licence`, `url`, `location`, `elevation`,
#' `collection`, `project` and `take` the file said anything about. `location`
#' is a latitude
#' and a longitude in degrees, and `datetime` is a POSIXct wherever the file
#' gave enough to make one.
#'
#' Where two formats say the same thing, the one that is believed is the first
#' of GUANO, wamd, the AudioMoth comment, bext, iXML, the INFO list, ID3,
#' Vorbis comments and the filename: a format written for bioacoustics is
#' trusted over one written for broadcast, and anything written inside the file
#' over anything read from its name. Nothing is thrown away by this, as
#' `sources` still holds what every format said.
#'
#' A field that holds only a year gives no `datetime`, as it has no month or
#' day to make one with; the year can still be read from the source that gave
#' it. Species are taken only from a format that names a species as such, and
#' never guessed at from a title.
#'
#' ## Reading from a name
#' `filename` reads the filename conventions that [parseFilename()] knows, and
#' is the one source nothing outranks: where the file itself said nothing, what
#' its name says is the whole answer. Pass `filename=FALSE` to read only what
#' is inside the file.
#'
#' Because nothing outranks it, a reading the name left in doubt gives no
#' `datetime`. A name of nothing but digits is read as a date or as a count of
#' seconds, and a collection that numbers its records gives names of exactly
#' that shape, so a date taken from one would often be a confident wrong answer
#' with nothing beside it to correct it.
#'
#' Where you know what your own files are named by, say so: `filename` takes
#' the name of a filename format as well as TRUE or FALSE, as in
#' `filename = "YYYYMMDD"`. Naming it settles what those digits mean, so the
#' date is read and used rather than passed over as a guess. The formats are
#' those [parseFilename()] lists.
#'
#' A TSA name gives a `species` and a `collection`. That corpus commonly holds
#' no embedded metadata at all, so for those recordings its name is the only
#' thing there is to read.
#'
#' @param x Path to an audio file, a Wave object with metadata attached, or a
#'   raw vector holding a file
#' @param filename Whether to read the name of the file as well as its
#'   contents. TRUE works the format out from the name, FALSE reads only what
#'   is inside the file, and the name of one of [parseFilename()]'s formats
#'   reads the name as that, which settles a name the package could not have
#'   settled on its own.
#' @return An audioMetadata object. `file` and `container` are always there, so
#'   its length says nothing about whether the file carried any metadata: ask
#'   `length(x$sources)` for that, or `length(x$fields)` for whether anything
#'   was read that this package has a normalised name for.
#' @export
#' @seealso The readers for one format at a time: [guano()], [wamd()],
#'   [bext()], [ixml()], [wavInfo()], [id3()], [vorbisComment()] and
#'   [audiomothWave()]. [`audioMetadata<-`] attaches a reading to a Wave object.
#' @examples
#' f <- system.file("extdata/AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
#' m <- readMetadata(f)
#' m$fields$model
#' m$sources$guano
#'
readMetadata <- function(x, filename=TRUE) {
  # Checked before anything is read, so that a format this package does not
  # know is an error whatever the file turns out to be.
  format <- .mdFilenameFormat(filename)
  if (inherits(x, "audioMetadata")) {
    return(x)
  }
  if (inherits(x, "Wave") || inherits(x, "WaveMC")) {
    return(.mdFromWaveAll(x))
  }
  if (is.raw(x)) {
    return(.mdFromBytesAll(x))
  }
  .mdCheckFile(x)
  return(.mdFromFileAll(x, filename=if (is.null(format)) FALSE else format))
}

#' Read every kind of metadata a file on disk carries
#' @keywords internal
#' @noRd
.mdFromFileAll <- function(file, filename=NA_character_) {
  # Each reader below looks for a chunk of its own, and each walked the file to
  # find it: a file carrying GUANO, wamd, bext, iXML, a list, an XMP packet, a
  # display name and an ID3 tag was opened and walked eight times over. The walk
  # is the same every time, so it is done once here and the readers are given
  # the map of it, which they take in place of a path. A file that is not a wave
  # file has no map, and those readers are not asked for one.
  map <- .mdWavMap(file)
  from <- if (is.null(map)) file else map
  container <- .mdContainer(.mdFileHead(from, 16L))
  sources <- list()
  unparsed <- character(0)

  if (.mdWaveContainer(container)) {
    sources$guano <- guano(from)
    sources$wamd <- wamd(from)
    sources$bext <- bext(from)
    sources$ixml <- ixml(from)
    sources$wavInfo <- wavInfo(from)
    sources$xmp <- xmp(from)
    sources$disp <- disp(from)
    # audiomothWave() reads the recording rather than a chunk of it, so it is
    # given the path whatever the map holds.
    sources$audiomoth <- .mdAudiomothSource(file, sources$wavInfo)
    unparsed <- .mdUnparsed(.mdWavChunkIds(from))
  }
  if (.mdWaveContainer(container) || identical(container, "MP3")) {
    sources$id3 <- id3(from)
  }
  if (identical(container, "FLAC") || identical(container, "Ogg")) {
    sources$vorbisComment <- vorbisComment(file)
    sources$id3 <- id3(file)
  }
  # FALSE to read no name at all, NA to work out the format from the name, or
  # the name of the format the caller knows their files follow.
  if (!isFALSE(filename)) {
    sources$filename <- .mdFilenameSource(
      file, format=if (is.na(filename)) NULL else filename
    )
  }
  return(.mdObjectAll(file, container, sources, unparsed))
}

#' Read every kind of metadata held in raw bytes
#' @keywords internal
#' @noRd
.mdFromBytesAll <- function(bytes) {
  container <- .mdContainer(bytes)
  sources <- list()
  unparsed <- character(0)

  if (.mdWaveContainer(container)) {
    sources$guano <- guano(bytes)
    sources$wamd <- wamd(bytes)
    sources$bext <- bext(bytes)
    sources$ixml <- ixml(bytes)
    sources$wavInfo <- wavInfo(bytes)
    sources$xmp <- xmp(bytes)
    sources$disp <- disp(bytes)
    unparsed <- .mdUnparsed(.mdWavChunkIds(bytes))
  }
  if (.mdWaveContainer(container) || identical(container, "MP3")) {
    sources$id3 <- id3(bytes)
  }
  if (identical(container, "FLAC") || identical(container, "Ogg")) {
    sources$vorbisComment <- vorbisComment(bytes)
    sources$id3 <- id3(bytes)
  }
  return(.mdObjectAll(NA_character_, container, sources, unparsed))
}

#' Record one format of metadata on a wave, leaving the others as they are
#'
#' Used by the setters for a single format, such as [`guano<-`], so that they
#' write into the one store a wave keeps rather than each keeping a place of
#' its own. The normalised record is built again from whatever the wave now
#' carries, so that setting a format changes what the wave says about itself.
#'
#' @param x A Wave or WaveMC object
#' @param name The name of the format being recorded
#' @param value The metadata of that format, or NULL to remove it
#' @return The wave, carrying the metadata
#' @keywords internal
#' @noRd
.mdSetSource <- function(x, name, value) {
  held <- .mdStore(x)
  sources <- if (is.null(held)) list() else held$sources
  sources[[name]] <- value
  if (length(sources) == 0L) {
    return(.mdSetStore(x, NULL))
  }
  return(.mdSetStore(x, .mdObjectAll(
    if (is.null(held)) NA_character_ else held$file,
    if (is.null(held)) NA_character_ else held$container,
    sources,
    if (is.null(held)) character(0) else held$unparsed
  )))
}

#' Read the metadata a Wave object carries
#' @keywords internal
#' @noRd
.mdFromWaveAll <- function(wave) {
  held <- .mdStore(wave)
  if (!is.null(held)) {
    return(held)
  }
  return(.mdObjectAll(NA_character_, NA_character_, list(), character(0)))
}

#' @export
print.audioMetadata <- function(x, ...) {
  if (!is.na(x$container)) {
    cat(x$container, "\n", sep="")
  }
  if (length(x$fields) == 0L && length(x$sources) == 0L) {
    cat("<no metadata>\n")
    return(invisible(x))
  }
  if (length(x$fields) > 0L) {
    width <- max(nchar(names(x$fields)))
    for (key in names(x$fields)) {
      cat(formatC(key, width=width, flag="-"), " ", .mdFieldText(x$fields[[key]]), "\n", sep="")
    }
  }
  if (length(x$sources) > 0L) {
    cat("\nread from: ", paste(names(x$sources), collapse=", "), "\n", sep="")
  }
  if (length(x$unparsed) > 0L) {
    cat("not read:  ", paste(x$unparsed, collapse=", "), "\n", sep="")
  }
  return(invisible(x))
}

#' Attach metadata to a Wave object
#'
#' A wave keeps the metadata of its file in one place, which [readMetadata()]
#' reads back, and where that place is depends on what it is. A TaggedWave has
#' a `metadata` slot made for this, so the reading goes in there, under the
#' name "audioMetadata" and beside anything else the caller has recorded in
#' that slot. A plain Wave has nowhere of its own, so an attribute of that same
#' name is used instead.
#'
#' [`guano<-`] writes into the same place, so a wave does not end up carrying
#' one record of its file in one place and another somewhere else.
#'
#' Writing the wave to a file does not carry the metadata with it: use
#' [guanoWrite()] on the written file, or pass the metadata to
#' [audiomothCompress()].
#'
#' @param x A Wave, WaveMC, TaggedWave or TaggedWaveMC object
#' @param value An audioMetadata object, from [readMetadata()], or NULL to
#'   remove what is attached
#' @return The object with the metadata attached
#' @export
#' @seealso [`guano<-`] attaches GUANO metadata on its own, in a form
#'   [guanoWrite()] can write back to a file.
#' @examples
#' \dontrun{
#' audioMetadata(wave) <- readMetadata("recording.wav")
#' readMetadata(wave)$fields$datetime
#' }
#'
`audioMetadata<-` <- function(x, value) {
  if (!inherits(x, "Wave") && !inherits(x, "WaveMC")) {
    stop("Metadata can only be attached to a Wave or WaveMC object.")
  }
  if (!is.null(value) && !inherits(value, "audioMetadata")) {
    stop("value must be an audioMetadata object, see readMetadata().")
  }
  return(.mdSetStore(x, value))
}
