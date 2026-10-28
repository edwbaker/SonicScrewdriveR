#' The mark that says a comment is a xeno-canto record
#'
#' The collection writes its own catalogue number at the head of every comment
#' it writes, followed by the copyright sign and the recordist, as
#' "XC428067 (c) A Recordist // ...". That mark is what makes the record safe
#' to read: a comment that does not begin with it is somebody else's and is
#' left alone. Asked of a cached copy of bio.acousti.ca and of TSA, 150,737
#' metadata values between them, nothing matched it anywhere.
#'
#' The copyright sign is part of the mark rather than merely the number, so
#' that a comment carrying a catalogue number and nothing else of the shape -
#' the first bytes of one, say, read off a truncated tag - is not taken for a
#' record it cannot fill. All 15,834 comments read from a cached copy of the
#' collection carry it, so requiring the sign costs nothing.
#'
#' The number is not taken from the filename. A recording downloaded from the
#' collection and uploaded somewhere else keeps the comment and gains a new
#' name, which is how three files in a cached copy of iNaturalist carry
#' xeno-canto records under iNaturalist numbers.
#' @keywords internal
#' @noRd
.XC_MARK <- "^XC[0-9]+[[:space:]]+\u00a9[[:space:]]"

#' Read the record a xeno-canto comment holds
#'
#' The comment is a fixed sequence of fields separated by " // ", and the first
#' four are always the catalogue number and recordist, the place, the
#' elevation and the time: of 15,834 comments read from a cached copy of the
#' collection, every one had them in that order. What follows varies, so it is
#' read by what it looks like rather than by where it sits: an "also:" field is
#' always last where there is one, and the rest is the recordist's remarks.
#'
#' @param text The comment string
#' @return A xenoCanto object holding the fields the comment gave, or NULL
#'   where the comment is not a xeno-canto record
#' @keywords internal
#' @noRd
.mdXenoCanto <- function(text) {
  if (is.null(text) || !length(text)) {
    return(NULL)
  }
  text <- trimws(as.character(text)[1])
  if (is.na(text) || !grepl(.XC_MARK, text)) {
    return(NULL)
  }
  # Some comments end in a separator with nothing after it, which would
  # otherwise leave the slashes on the end of the last field read.
  text <- sub("[[:space:]]*/+[[:space:]]*$", "", text)
  parts <- trimws(strsplit(text, " // ", fixed=TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  f <- list()

  # 1: the catalogue number and, after the copyright sign, the recordist.
  f$id <- as.integer(sub("^XC([0-9]+).*$", "\\1", parts[1]))
  f$recordist <- .mdXcNonEmpty(sub(
    paste0("^XC[0-9]+[[:space:]]*\u00a9[[:space:]]*"), "", parts[1]
  ))

  # 2: the place, with the position in brackets where the recordist gave one
  # and the country after it. 4% of 15,834 gave no position.
  if (length(parts) >= 2) {
    place <- .mdXcPlace(parts[2])
    f$locality <- place$locality
    f$latitude <- place$latitude
    f$longitude <- place$longitude
    f$country <- place$country
  }

  # 3: the elevation in metres. It is a range ("200 - 500m") or a placeholder
  # ("?m", "-m", "m") often enough that the text is kept beside the number.
  if (length(parts) >= 3) {
    f$stated_elevation <- .mdXcNonEmpty(parts[3])
    f$elevation <- .mdXcElevation(parts[3])
  }

  # 4: the time and the date. The date is written to a pattern; the time is
  # whatever the recordist typed, and of 15,834 comments some hundreds held
  # something no pattern reads ("amh", "10;45amh", "xx:xxh"). They are read
  # apart so that a time nothing can make sense of does not cost the date
  # beside it.
  if (length(parts) >= 4) {
    f$time <- .mdXcNonEmpty(sub("h,.*$", "", parts[4]))
    f$date <- .mdXcNonEmpty(sub("^.*h,[[:space:]]*", "", parts[4]))
  }

  # 5 onwards: the kind of vocalisation, then the remarks, then the other
  # species audible in the recording.
  if (length(parts) >= 5) {
    f$type <- .mdXcNonEmpty(parts[5])
  }
  rest <- if (length(parts) >= 6) parts[6:length(parts)] else character(0)
  also <- grepl("^also:", rest)
  if (any(also)) {
    f$also <- .mdXcList(sub("^also:[[:space:]]*", "", rest[also][1]))
  }
  rest <- rest[!also]
  if (length(rest)) {
    said <- .mdXcRemarks(paste(rest, collapse=" // "))
    f$remarks <- said$remarks
    f$bird_seen <- said$bird_seen
    f$playback_used <- said$playback_used
  }

  f <- f[!vapply(f, is.null, logical(1))]
  return(.mdObject(f, "xenoCanto"))
}

#' A value, or NULL where it says nothing
#'
#' The collection writes a question mark, or a run of x, where the recordist
#' gave nothing, and those are an absence rather than a reading of one.
#' @keywords internal
#' @noRd
.mdXcNonEmpty <- function(x) {
  x <- trimws(x)
  if (!length(x) || is.na(x) || !nzchar(x)) {
    return(NULL)
  }
  if (grepl("^[?x.\u00bf -]+$", x, ignore.case=TRUE)) {
    return(NULL)
  }
  return(x)
}

#' The place field, as locality, position and country
#' @keywords internal
#' @noRd
.mdXcPlace <- function(text) {
  out <- list(locality=NULL, latitude=NULL, longitude=NULL, country=NULL)
  m <- regmatches(text, regexec(
    "^(.*?)[[:space:]]*\\((-?[0-9.]+),[[:space:]]*(-?[0-9.]+)\\)[[:space:]]*,?[[:space:]]*(.*)$",
    text
  ))[[1]]
  if (length(m) == 5L) {
    lat <- suppressWarnings(as.numeric(m[3]))
    lon <- suppressWarnings(as.numeric(m[4]))
    out$locality <- .mdXcNonEmpty(m[2])
    # A position outside the range of the units it is written in is not a
    # position, whatever it is: it is left out rather than reported as one.
    if (!is.na(lat) && !is.na(lon) && abs(lat) <= 90 && abs(lon) <= 180) {
      out$latitude <- lat
      out$longitude <- lon
    }
    out$country <- .mdXcNonEmpty(m[5])
    return(out)
  }
  # No position: the country is what follows the last comma.
  if (grepl(",", text, fixed=TRUE)) {
    out$country <- .mdXcNonEmpty(sub("^.*,[[:space:]]*", "", text))
    out$locality <- .mdXcNonEmpty(sub(",[^,]*$", "", text))
  } else {
    out$locality <- .mdXcNonEmpty(text)
  }
  return(out)
}

#' The elevation in metres, where the field holds one number
#' @keywords internal
#' @noRd
.mdXcElevation <- function(text) {
  m <- regmatches(text, regexec("^(-?[0-9]+(?:\\.[0-9]+)?)[[:space:]]*m$", trimws(text)))[[1]]
  if (length(m) != 2L) {
    return(NULL)
  }
  return(as.numeric(m[2]))
}

#' The remarks, and the two flags the collection writes among them
#'
#' The flags are written as "bird-seen:yes" between slashes, which the
#' recordist's own text runs either side of.
#' @keywords internal
#' @noRd
.mdXcRemarks <- function(text) {
  out <- list(remarks=NULL, bird_seen=NULL, playback_used=NULL)
  flag <- function(name) {
    m <- regmatches(text, regexec(
      paste0(name, ":[[:space:]]*(yes|no|unknown)"), text, ignore.case=TRUE
    ))[[1]]
    if (length(m) != 2L) {
      return(NULL)
    }
    return(switch(tolower(m[2]), yes=TRUE, no=FALSE, NULL))
  }
  out$bird_seen <- flag("bird-seen")
  out$playback_used <- flag("playback-used")
  left <- gsub("(bird-seen|playback-used|img):[^/]*", "", text, ignore.case=TRUE)
  left <- gsub("[[:space:]]*/[[:space:]]*", " ", left)
  out$remarks <- .mdXcNonEmpty(gsub("[[:space:]]+", " ", left))
  return(out)
}

#' The clock a time field gives, as "HH:MM:SS", or NULL where it gives none
#'
#' The field is whatever the recordist typed. Most of it is a 24-hour clock,
#' but of 15,834 comments read from a cached copy of the collection 158 carried
#' am or pm, and 30 of those are an afternoon time written before noon, as
#' "2:00 pm" is, which a reader that ignored the marker would place twelve
#' hours early. The marker is read where it is there, and a
#' field holding no clock at all gives nothing rather than a guess.
#' @keywords internal
#' @noRd
.mdXcClock <- function(text) {
  if (is.null(text) || !length(text) || is.na(text)) {
    return(NULL)
  }
  m <- regmatches(text, regexec(
    "([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?", text
  ))[[1]]
  if (length(m) < 3L) {
    return(NULL)
  }
  hour <- suppressWarnings(as.integer(m[2]))
  minute <- suppressWarnings(as.integer(m[3]))
  second <- if (length(m) >= 4L && nzchar(m[4])) suppressWarnings(as.integer(m[4])) else 0L
  if (is.na(hour) || is.na(minute) || is.na(second) ||
      minute > 59L || second > 59L) {
    return(NULL)
  }
  if (grepl("p\\.?m", text, ignore.case=TRUE) && hour < 12L) {
    hour <- hour + 12L
  } else if (grepl("a\\.?m", text, ignore.case=TRUE) && hour == 12L) {
    hour <- 0L
  }
  if (hour > 23L) {
    return(NULL)
  }
  return(sprintf("%02d:%02d:%02d", hour, minute, second))
}

#' A comma separated field as the values it holds
#' @keywords internal
#' @noRd
.mdXcList <- function(text) {
  v <- trimws(strsplit(text, ",", fixed=TRUE)[[1]])
  v <- v[nzchar(v)]
  if (!length(v)) {
    return(NULL)
  }
  return(v)
}

#' The xeno-canto record a tag carries, if it carries one
#'
#' The comment is wherever the tag keeps one: a version 2 COMM frame, the TXXX
#' frame the collection writes in its newer files, or the fixed comment field
#' of a version 1 tag.
#' @keywords internal
#' @noRd
.mdXenoCantoSource <- function(tag) {
  comment <- c(
    .mdPickPrefix(tag, "COMM"), .mdPick(tag, "comment"),
    .mdPickPrefix(tag, "TXXX|comment")
  )
  for (text in comment) {
    read <- .mdXenoCanto(text)
    if (.mdHasFields(read)) {
      return(read)
    }
  }
  return(NULL)
}

#' The normalised fields a xeno-canto record gives
#'
#' The time the comment gives is a wall clock at the place of the recording,
#' with no zone beside it, so it is read as one in the zone of the session, as
#' a time in an INFO list is. A comment that gave no time, or gave one in a
#' form nothing reads, gives no datetime: the date is still in the record, and
#' a midnight nobody wrote would be a reading of the file rather than of what
#' it says.
#'
#' The species in "also:" are the others audible in the recording rather than
#' the one it is of, so they are not reported as the species of the file.
#' @keywords internal
#' @noRd
.mdNormaliseXenoCanto <- function(x) {
  date <- .mdPick(x, "date")
  time <- .mdPick(x, "time")
  datetime <- NULL
  # A date the collection writes with a zero month or day is a date it was not
  # told, and there is no day to make a time on.
  if (!is.null(date) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date) &&
      !grepl("-00", date, fixed=TRUE) && !startsWith(date, "0000")) {
    clock <- .mdXcClock(time)
    if (!is.null(clock)) {
      datetime <- .mdDate(paste0(date, " ", clock))
    }
  }
  position <- c(.mdPick(x, "latitude"), .mdPick(x, "longitude"))
  return(list(
    datetime = datetime,
    recordist = .mdPick(x, "recordist"),
    note = .mdPick(x, "remarks"),
    collection = "xeno-canto",
    location = if (length(position) == 2L) {
      c(latitude=unname(position[1]), longitude=unname(position[2]))
    } else {
      NULL
    },
    elevation = .mdPick(x, "elevation")
  ))
}
