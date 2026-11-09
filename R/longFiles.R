#' How much memory a file takes once it is read, worked out from its header
#'
#' The samples of a wave are what grows with the length of a recording, and R
#' holds them as integers of four bytes or as doubles of eight, depending on
#' the reader: readWave() gives integers for PCM of 8, 16 and 32 bits and
#' doubles for 24-bit PCM and floating point, MP3 is given as 16-bit integers,
#' and anything else the av package decodes, FLAC among them, comes back as
#' doubles. A
#' WAV file is measured from its chunks, and one whose audio size was never
#' written from the bytes after its data chunk begins, as readAudio() reads it
#' to the end of the file. A triggered recording is measured as it will be once
#' expanded. Anything else is asked of the av package, and where
#' that is not installed nothing can be said of its size without decoding it.
#'
#' Whether the file can be read a window at a time is said too. Only a RIFF
#' WAV file of PCM or floating-point audio can: readAudio() reads a part of it
#' with readWave(), which gives the samples a whole read gives, where a part of
#' anything else is decoded from the start of the file, or from the codec frame
#' before it, and is not the same.
#' @param file Path to an audio file.
#' @return `NULL` where the size cannot be told, or a list of `bytes`, the
#'   `channels`, the sample `rate`, the `perSample` bytes, and `reason`, why the
#'   file cannot be read a window at a time, or `NA` where it can.
#' @keywords internal
#' @noRd
.decodedSize <- function(file) {
  mime <- .audioMime(file)
  if (identical(mime, "audio/x-wav")) {
    info <- tryCatch(.wavChunksFile(file), error=function(e) NULL)
    data <- if (is.null(info)) NULL else .wavFindChunk(info$chunks, "data")
    fmt <- tryCatch(.wavChunkFromFile(file, "fmt "), error=function(e) NULL)
    fields <- if (is.null(fmt)) NULL else .wavFmtFields(fmt, length(fmt))
    #The formats .wavFramesHeld() counts, which readWave() reads.
    if (!is.null(data) && !is.null(fields) && fields$format %in% c(0, 1, 3, 65534) &&
        fields$frameSize > 0) {
      riff <- info$form == "RIFF"
      #A size of zero is the size of the audio only where the RIFF size and what
      #follows say that it was never written, as readAudio() decides it.
      held <- if (data$size != 0) data$size else {
        peek <- function(at, n) {
          con <- file(file, "rb")
          on.exit(close(con))
          return(.wavConReader(con)(at, n))
        }
        .wavUnfinalisedSize(data, info$riffSize, fields$frameSize, file.size(file), peek)
      }
      #audiomothExpand() expands only the mono 16-bit PCM an AudioMoth writes,
      #in any of the three forms, before anything else reads it, and
      #readAudio() reads any other recording as it is.
      triggered <- .isExpandable(file)
      if (triggered) {
        held <- held + .triggeredGrowth(file, data$start, held)
      }
      #audiomothExpand() gives the samples as integers, as readWave() gives those
      #of PCM of 8, 16 and 32 bits.
      integer <- triggered || (riff && fields$format != 3 && fields$bits %in% c(8, 16, 32))
      per <- if (integer) 4 else 8
      reason <- if (triggered) {
        "an AudioMoth triggered recording is expanded whole before it is read"
      } else if (!riff) {
        paste("a WAV file in the", info$form, "form is read whole, by the av package")
      } else {
        NA_character_
      }
      return(list(
        bytes=(held %/% fields$frameSize) * fields$channels * per,
        channels=fields$channels, rate=fields$sampleRate, perSample=per, reason=reason
      ))
    }
  }
  if (!package.installed("av", askInstall=FALSE)) {
    return(NULL)
  }
  info <- tryCatch(av::av_media_info(file), error=function(e) NULL)
  if (is.null(info) || NROW(info$audio) == 0 || is.na(info$duration)) {
    return(NULL)
  }
  per <- if (identical(mime, "audio/mpeg")) 4 else 8
  rate <- info$audio$sample_rate[1]
  channels <- info$audio$channels[1]
  return(list(
    bytes=round(info$duration * rate) * channels * per,
    channels=channels, rate=rate, perSample=per,
    reason=if (identical(mime, "audio/x-wav")) {
      "only a WAV file of PCM or floating-point audio is read a window at a time"
    } else {
      "only a WAV file is read a window at a time, as a part of any other format is not read exactly"
    }
  ))
}

#' How many bytes expanding a triggered recording adds to its audio
#'
#' audiomothExpand() replaces each encoded block with the blocks of silence it
#' counts, so the expanded audio is that many blocks longer, less the encoded
#' block itself. Only a block wholly within the audio counts, as it does for
#' audiomothExpand(): some 512 bytes of a chunk after the audio can read as an
#' encoded block, and are left as they are. The blocks are counted a few
#' megabytes at a time, so a recording too long to hold is measured without
#' being held.
#' @param file A triggered recording.
#' @param start The one-based position of the first byte of its audio.
#' @param size The bytes of audio it holds, recovered where its size was never
#'   written.
#' @param chunk How many blocks to read at a time.
#' @return A number of bytes.
#' @keywords internal
#' @noRd
.triggeredGrowth <- function(file, start, size, chunk=2^14) {
  blocks <- file.size(file) %/% .AM_BLOCK_SIZE
  con <- file(file, "rb")
  on.exit(close(con))
  grown <- 0
  done <- 0
  while (done < blocks) {
    n <- min(chunk, blocks - done)
    bytes <- readBin(con, "raw", n=n * .AM_BLOCK_SIZE)
    counts <- .audiomothBlockCounts(matrix(bytes, nrow=.AM_BLOCK_SIZE))
    first <- (done + seq_len(n) - 1) * .AM_BLOCK_SIZE + 1
    inside <- first >= start & first + .AM_BLOCK_SIZE - 1 <= start + size - 1
    grown <- grown + sum(counts[inside & counts > 0] - 1)
    done <- done + n
  }
  return(grown * .AM_BLOCK_SIZE)
}

#' A number of bytes as a person reads it
#' @param x Bytes.
#' @return A string, as "4 GB".
#' @keywords internal
#' @noRd
.bytesText <- function(x) {
  return(format(structure(x, class="object_size"), units="auto", standard="SI"))
}

#' Why a file over max.memory cannot be run, if it cannot
#'
#' A file over the limit is read a window at a time, so it can be run only
#' where it can be read that way and every step at the top of the pipeline is a
#' windowing given then=: any other step needs the whole recording at once. A
#' window that is itself over the limit cannot be held either. All of this is
#' known from the header and the pipeline, so the file fails before anything is
#' read.
#' @param p The pipeline.
#' @param size What `.decodedSize()` gives for the file.
#' @param limit `max.memory`.
#' @return A message, or `NULL` where the file can be run.
#' @keywords internal
#' @noRd
.longFileProblem <- function(p, size, limit) {
  over <- paste0(
    "At ", .bytesText(size$bytes), " decoded, the file is over max.memory (",
    .bytesText(limit), ")"
  )
  if (!is.na(size$reason)) {
    return(paste0(over, ", and ", size$reason, "."))
  }
  for (i in seq_along(p$steps)) {
    s <- p$steps[[i]]
    call <- .deparseOne(s$call)
    if (is.null(s$then) || !.takesThen(s$fun)) {
      return(paste0(
        over, ", and step ", i, " ", call, " needs all of it at once: only the ",
        "steps of a windowing() given then= are run on such a file, a window at a time."
      ))
    }
    window <- .windowFrames(s, size$rate) * size$channels * size$perSample
    if (window > limit) {
      return(paste0(
        "A window of step ", i, " ", call, " takes ", .bytesText(window),
        " decoded, over max.memory (", .bytesText(limit), ")."
      ))
    }
  }
  return(NULL)
}

#' How many sample frames each window of a windowing step holds
#'
#' A length windowing() would refuse is left for it to refuse, with its own
#' message, once the file is run, where a failure is a failure of that file
#' rather than of the batch.
#' @param s A windowing step.
#' @param rate The sample rate of the file.
#' @return A number of frames.
#' @keywords internal
#' @noRd
.windowFrames <- function(s, rate) {
  given <- .describeCall(s$fun, s$args)$params
  defaults <- formals(windowing)
  len <- if (is.null(given$window.length)) defaults$window.length else given$window.length
  unit <- if (is.null(given$time.unit)) defaults$time.unit else given$time.unit
  frames <- tryCatch(.inSamples(len, unit, rate), error=function(e) NA)
  #Anything but one number, as NA or two lengths, is no length of a window.
  if (!is.numeric(frames) || length(frames) != 1 || is.na(frames)) {
    return(0)
  }
  return(frames)
}

#' Stand in for a file too long to read whole
#'
#' A tagged wave holding no samples but the sample rate, bit depth and channels
#' of the file, the tags a whole read would give it, and the fields
#' calculateMetadata() would calculate, its frames counted from the chunks of
#' the file and the digest of its samples taken a block at a time. The first
#' sample frame is read by readAudio(), so that what a whole read says of the
#' file is said once, as it would be, and the wave records the file as a whole
#' read records it.
#'
#' A file readAudio() reads from a repaired copy, as one whose audio size was
#' never written, is copied once here, and its first frame and its windows are
#' read from the copy, rather than each read copying the file again. What
#' readAudio() says of such a file, in the order it says it, is said here of
#' the file rather than of the copy.
#' @param path The file.
#' @param origin What to tag it with as its origin.
#' @param metadata What the recordings of the set agree on.
#' @param block How many sample frames to read at a time for the digest, as
#'   `.hashBlock()` gives it.
#' @return A list of `wave`, and `file`: the `path` to read windows from, the
#'   `copy` to delete once the file has been run, or `NULL`, the number of
#'   `frames`, and `hash`, a function of a hash method giving the digest of the
#'   whole recording, for the records of its windows.
#' @keywords internal
#' @noRd
.longFileWave <- function(path, origin, metadata, block=2^20) {
  truncation <- .wavTruncation(path)
  copy <- .wavRepaired(path, truncation)
  kept <- FALSE
  if (is.null(copy)) {
    one <- readAudio(path, from=1, to=1, time.unit="samples")
  } else {
    on.exit(if (!kept) unlink(copy), add=TRUE)
    if (!is.null(truncation)) {
      .truncatedWarning(truncation, path)
    }
    one <- .quietRead(copy, 1, 1)
    if (is.null(truncation)) {
      .unwrittenWarning(path)
    }
    .wavRateWarning(one, path)
    one <- .recordFile(one, path)
  }
  source <- if (is.null(copy)) path else copy
  frames <- .wavFramesHeld(source)$frames
  multi <- methods::is(one, "WaveMC")
  channels <- if (multi) ncol(one@.Data) else if (one@stereo) 2L else 1L
  empty <- one
  if (multi) {
    empty@.Data <- one@.Data[0, , drop=FALSE]
  } else {
    empty@left <- one@left[0]
    empty@right <- one@right[0]
  }
  w <- tagWave(empty, origin=origin)
  w@metadata[["recordingSet"]] <- metadata
  #Taken in full, as calculateMetadata() takes it, and kept for the records of
  #the windows that ask for it.
  hashes <- list(full=.fileWaveHash(source, "full", one, frames, block))
  w@metadata[["calculatedMetadata"]] <- .calculatedFields(w, frames, channels, hashes$full)
  hash <- function(method) {
    if (is.null(hashes[[method]])) {
      hashes[[method]] <<- .fileWaveHash(source, method, one, frames, block)
    }
    return(hashes[[method]])
  }
  kept <- TRUE
  return(list(wave=w, file=list(path=source, copy=copy, frames=frames, hash=hash)))
}

#' How many sample frames to read at a time for a digest
#'
#' A million frames, a few megabytes, unless fewer than that fit in
#' max.memory, so that no block read takes more than it decoded.
#' @param size What `.decodedSize()` gives for the file.
#' @param limit `max.memory`.
#' @return A number of frames, at least 1.
#' @keywords internal
#' @noRd
.hashBlock <- function(size, limit) {
  return(max(1, min(2^20, floor(limit / (size$channels * size$perSample)))))
}

#' The digest .waveHash() gives of a whole recording, read from disk
#'
#' The parts .waveParts() gives a wave are each digested as .digestNumeric()
#' digests them: the samples of each channel of a Wave, or of every channel of
#' a WaveMC one after the other, then its dimensions, then the sample rate and
#' bit depth. A full digest writes the samples to a file a block at a time, as
#' the bytes .digestNumeric() would digest, and digests the file, so the memory
#' it takes does not grow with the recording. A sketch reads only the blocks
#' .waveSketch() takes, which it holds, at most 40,960 samples. Neither reads
#' more than `block` frames at once.
#' @param path The file, which readAudio() reads in parts.
#' @param method "full" or "sketch".
#' @param one Its first sample frame, as readAudio() reads it.
#' @param frames The sample frames it holds.
#' @param block How many sample frames to read at a time.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.fileWaveHash <- function(path, method, one, frames, block=2^20) {
  multi <- methods::is(one, "WaveMC")
  channels <- if (multi) ncol(one@.Data) else if (one@stereo) 2L else 1L
  #A part read short, as from a file cut short since it was measured, would give
  #the digest of fewer samples than the recording holds, so it fails instead.
  read <- function(from, to) {
    w <- .quietRead(path, from, to)
    held <- if (multi) nrow(w@.Data) else length(w@left)
    if (held != to - from + 1) {
      stop("Could not read sample frames ", from, " to ", to,
           " of the recording to digest them.")
    }
    return(w)
  }
  column <- function(w, ch) {
    if (multi) {
      return(w@.Data[, ch])
    }
    return(if (ch == 1) w@left else w@right)
  }
  #The samples of one channel from one frame to another, read no more than a
  #block at a time.
  values <- function(ch, first, last) {
    out <- list()
    from <- first
    while (from <= last) {
      to <- min(last, from + block - 1)
      out[[length(out) + 1]] <- as.numeric(column(read(from, to), ch))
      from <- to + 1
    }
    return(unlist(out))
  }
  #The values of one part, from position a to position b. A WaveMC part holds
  #every channel in turn, so a run of it can begin in one channel and end in
  #the next.
  part <- function(k, a, b) {
    if (!multi) {
      return(values(k, a, b))
    }
    out <- list()
    while (a <= b) {
      ch <- (a - 1) %/% frames + 1
      first <- (a - 1) %% frames + 1
      last <- min(frames, first + (b - a))
      out[[length(out) + 1]] <- values(ch, first, last)
      a <- a + (last - first + 1)
    }
    return(unlist(out))
  }
  sampled <- if (multi) 1L else seq_len(channels)
  lengths <- if (multi) frames * channels else rep(frames, channels)
  tail <- list(if (multi) c(frames, channels), c(one@samp.rate, one@bit))
  if (!multi && channels == 1) {
    tail <- c(list(numeric(0)), tail)
  }
  tail <- Filter(Negate(is.null), tail)
  if (identical(method, "sketch")) {
    #The blocks .waveSketch() takes by default.
    n <- formals(.waveSketch)$n
    sketches <- lapply(seq_along(sampled), function(k) {
      starts <- .sketchStarts(lengths[k], n)
      if (is.null(starts)) {
        return(part(sampled[k], 1, lengths[k]))
      }
      return(unlist(lapply(starts, function(s) part(sampled[k], s, s + n - 1))))
    })
    digests <- vapply(
      c(list(c(lengths, vapply(tail, length, numeric(1)))), sketches, lapply(tail, .waveSketch)),
      .digestNumeric, character(1)
    )
  } else {
    digests <- c(.streamedDigests(read, column, frames, channels, multi, block),
                 vapply(tail, .digestNumeric, character(1)))
  }
  return(digest::digest(paste0(digests, collapse=""), algo="sha256", serialize=FALSE))
}

#' The digest of the samples of each part of a recording, read a block at a time
#' @param read A function reading the frames from one position to another.
#' @param column A function taking one channel from what `read` gives.
#' @param frames,channels The size of the recording.
#' @param multi Whether it is read as a WaveMC object, whose channels are one
#'   part.
#' @param block How many sample frames to read at a time.
#' @return One digest for each part that holds samples.
#' @keywords internal
#' @noRd
.streamedDigests <- function(read, column, frames, channels, multi, block) {
  files <- vapply(seq_len(channels), function(ch) tempfile(fileext=".bin"), character(1))
  on.exit(unlink(files), add=TRUE)
  #The next block is found from the last, rather than every block being listed
  #first, which for a small block of a long recording would take more memory
  #than the blocks themselves.
  from <- 1
  while (from <= frames) {
    to <- min(frames, from + block - 1)
    w <- read(from, to)
    for (ch in seq_len(channels)) {
      .appendSamples(files[ch], as.numeric(column(w, ch)))
    }
    from <- to + 1
  }
  #A write cut short, as by a full disk, need not fail, and would give the
  #digest of part of the recording, so each file is measured: every sample is
  #written as a double of 8 bytes.
  if (!isTRUE(all(file.size(files) == 8 * frames))) {
    stop("Could not write the samples of the recording to disk to digest them.")
  }
  digested <- files
  if (multi) {
    #So with a join cut short. Each channel is deleted once it has been joined,
    #so the disk holds no more than one channel twice over.
    for (ch in seq_len(channels)[-1]) {
      if (!.appendFile(files[1], files[ch])) {
        stop("Could not join the channels of the recording to digest them.")
      }
      unlink(files[ch])
    }
    if (!isTRUE(file.size(files[1]) == 8 * frames * channels)) {
      stop("Could not join the channels of the recording to digest them.")
    }
    digested <- files[1]
  }
  return(vapply(digested, function(f) digest::digest(file=f, algo="sha256"), character(1),
                USE.NAMES=FALSE))
}

#' Append samples to a file, as the bytes .digestNumeric() would digest
#'
#' The file is open only while they are written. A recording can have more
#' channels than R can hold connections open at once, and a connection left
#' open by a failed write would take one from every recording read after, and
#' on Windows keep the file from being deleted.
#' @param path The file, which is made if there is none.
#' @param x The samples, as doubles.
#' @return Nothing.
#' @keywords internal
#' @noRd
.appendSamples <- function(path, x) {
  con <- file(path, open="ab")
  on.exit(close(con))
  writeBin(x, con)
  return(invisible(NULL))
}

#' Append one file to another
#' @param to The file to append to.
#' @param from The file to append.
#' @return Whether it was appended.
#' @keywords internal
#' @noRd
.appendFile <- function(to, from) {
  return(file.append(to, from))
}

#' Read part of a file whose warnings have been said already
#'
#' What a whole read says of a file, that it was cut short or that its header
#' disagrees with its metadata, was said once when the first frame was read,
#' and is not said again for every part read after. Samples that are not
#' finite are said for each part that holds them, as they are of that part.
#' @param path The file.
#' @param from,to The first and last sample frames.
#' @return A Wave or WaveMC object.
#' @keywords internal
#' @noRd
.quietRead <- function(path, from, to) {
  return(withCallingHandlers(
    readAudio(path, from=from, to=to, time.unit="samples"),
    audioFileWarning=.muffleSaid
  ))
}

#' Muffle a warning about a file that a whole read would have said once
#' @param cond The warning.
#' @keywords internal
#' @noRd
.muffleSaid <- function(cond) {
  if (!inherits(cond, "nonFiniteSamples")) {
    invokeRestart("muffleWarning")
  }
}

#' Run a windowing step on a file too long to read whole
#'
#' windowing() given the path reads each window from the file. Each is tagged
#' as the window cut from the whole wave would be: the tags of the wave, then a
#' record of the cut, digesting the whole recording as the wave going in.
#' Warnings a whole read would have said once are not said for each window,
#' but anything the steps run on a window say is.
#' @param s The windowing step.
#' @param w The wave standing for the file, as `.longFileWave()` gives it.
#' @param file The file, as `.longFileWave()` gives it.
#' @param each The function to run on each tagged window, given it, its start
#'   and its length.
#' @return What windowing() gives.
#' @keywords internal
#' @noRd
.fileWindows <- function(s, w, file, each) {
  reading <- TRUE
  fromFile <- function(section, start, len) {
    reading <<- FALSE
    on.exit(reading <<- TRUE)
    return(each(.fileWindow(w, section, start, file$hash), start, len))
  }
  return(withCallingHandlers(
    do.call(s$fun, c(list(file$path), s$args, list(FUN=fromFile))),
    audioFileWarning=function(cond) {
      if (reading) {
        .muffleSaid(cond)
      }
    }
  ))
}

#' Tag a window read from a file as the window cut from the whole wave
#'
#' cutws() records the samples it cut to, so the last sample is the one the
#' window ends at, which for a last window shorter than the rest is the last of
#' the recording.
#' @param w The wave standing for the file.
#' @param section The window, as readAudio() read it.
#' @param start Its first sample frame in the file.
#' @param hash A function of a hash method giving the digest of the whole
#'   recording.
#' @return A tagged wave.
#' @keywords internal
#' @noRd
.fileWindow <- function(w, section, start, hash) {
  ran <- .tagApplyRecord(
    w, function(wave, from, to, plot) section, "cutws", NULL, TRUE,
    from=start, to=start + length(section) - 1, plot=FALSE,
    .described=.cutws, .inputHash=hash
  )
  return(.keepRecord(w, ran$result, ran$record))
}
