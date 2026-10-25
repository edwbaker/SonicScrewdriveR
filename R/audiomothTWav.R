# Support for the AudioMoth triggered (T.WAV) file format, as described in the
# Open Acoustic Devices application note "AudioMoth Triggered T.WAV File Format"
# (4th January 2021) and implemented by the AudioMoth firmware.
#
# A T.WAV file is an ordinary mono 16-bit PCM WAV file in which the periods that
# did not trigger a recording have been left out. Each omitted period is
# replaced by a single 512-byte encoded block whose first thirty-two 16-bit
# samples are -1 or 1, giving the bits (least significant first) of the number
# of 512-byte blocks that were skipped, and whose remaining samples are zero.
# Conventional WAV tools read an encoded block as a very short, and effectively
# silent, run of samples.

.AM_BLOCK_SIZE <- 512L
.AM_HEADER_SIZE <- 488L
.AM_SEGMENT_SIZE <- 32768L
.AM_ICMT_SIZE <- 384L
.AM_IART_SIZE <- 32L
.AM_LIST_SIZE <- 436L

#' Does a filename follow the AudioMoth convention for triggered recordings?
#'
#' @param file A filename
#' @return A logical value
#' @keywords internal
#' @noRd
.isTWavName <- function(file) {
  grepl("T\\.WAV$", file, ignore.case=TRUE)
}

#' Find the encoded blocks of a T.WAV file
#'
#' @param blocks A raw matrix with one 512-byte block per column
#' @return For each block, the number of 512-byte blocks it encodes, or zero if
#'   it is not an encoded block
#' @keywords internal
#' @noRd
.audiomothBlockCounts <- function(blocks) {
  nBlocks <- ncol(blocks)
  counts <- numeric(nBlocks)
  if (nBlocks == 0L) {
    return(counts)
  }

  # Only a few blocks can be encoded blocks: the first sample must be -1 or 1,
  # and the last must be zero. Screening on those bytes avoids decoding every
  # sample of the file.
  first <- as.integer(blocks[1L, ])
  second <- as.integer(blocks[2L, ])
  penultimate <- as.integer(blocks[.AM_BLOCK_SIZE - 1L, ])
  last <- as.integer(blocks[.AM_BLOCK_SIZE, ])
  candidate <- which(
    (first == 1L | first == 255L) &
      (second == 0L | second == 255L) &
      penultimate == 0L & last == 0L
  )
  if (length(candidate) == 0L) {
    return(counts)
  }

  perBlock <- .AM_BLOCK_SIZE %/% 2L
  samples <- matrix(
    readBin(
      as.vector(blocks[, candidate, drop=FALSE]), "integer",
      n=length(candidate) * perBlock, size=2L, signed=TRUE, endian="little"
    ),
    nrow=perBlock
  )
  flags <- samples[1:32, , drop=FALSE]
  encoded <- colSums(flags != 1L & flags != -1L) == 0L &
    colSums(samples[33:perBlock, , drop=FALSE] != 0L) == 0L
  if (any(encoded)) {
    counts[candidate[encoded]] <- as.vector(
      2^(0:31) %*% (flags[, encoded, drop=FALSE] == 1L)
    )
  }

  # A block encoding zero skipped blocks is not an encoded block, and is copied
  # through unchanged, as the reference implementation does.
  return(counts)
}

#' Plan the expansion of a T.WAV file
#'
#' Describes the expanded file as a sequence of steps: runs of blocks carried
#' over unchanged, runs of silence replacing an encoded block, and the bytes
#' after the last whole block. A plan can then either be written to a connection
#' or used to fill a vector, without the two going out of step.
#'
#' @param counts Encoded block counts from .audiomothBlockCounts()
#' @param tailBytes Number of bytes after the last whole block
#' @return A list giving, for each step, where it reads from, the first and last
#'   input byte, its position in the expanded file and its size, along with the
#'   size of the expanded file
#' @keywords internal
#' @noRd
.audiomothExpandPlan <- function(counts, tailBytes) {
  nBlocks <- length(counts)
  encoded <- counts > 0
  runs <- rle(!encoded)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  steps <- sum(ifelse(runs$values, 1L, runs$lengths)) + (tailBytes > 0)
  source <- character(steps)
  from <- numeric(steps)
  to <- numeric(steps)
  size <- numeric(steps)

  step <- 0L
  for (k in seq_along(runs$lengths)) {
    if (runs$values[k]) {
      step <- step + 1L
      source[step] <- "blocks"
      from[step] <- (starts[k] - 1) * .AM_BLOCK_SIZE + 1
      # Both of these are whole numbers, and a recording of 2GB or more holds
      # more blocks than their product can be counted in, so the arithmetic is
      # done in doubles rather than silently giving NA.
      to[step] <- as.numeric(ends[k]) * .AM_BLOCK_SIZE
      size[step] <- to[step] - from[step] + 1
    } else {
      # Runs of encoded blocks are rare, and almost always a single block.
      for (block in starts[k]:ends[k]) {
        step <- step + 1L
        source[step] <- "blank"
        size[step] <- counts[block] * .AM_BLOCK_SIZE
      }
    }
  }
  if (tailBytes > 0) {
    step <- step + 1L
    source[step] <- "tail"
    from[step] <- 1
    to[step] <- tailBytes
    size[step] <- tailBytes
  }

  return(list(
    source = source,
    from = from,
    to = to,
    size = size,
    at = cumsum(c(1, size))[seq_len(steps)],
    total = sum(size)
  ))
}

#' Expand an AudioMoth triggered (T.WAV) file
#'
#' AudioMoth devices recording on a trigger, such as an amplitude threshold,
#' write a single T.WAV file in which the periods that did not trigger a
#' recording are left out. Each omitted period is replaced by a 512-byte encoded
#' block recording its length, so that the original timing of the triggered
#' recordings can be restored. This function restores them, returning either a
#' Wave object or an ordinary WAV file in which the omitted periods are silence.
#'
#' Only mono 16-bit PCM files are supported, which is what AudioMoth devices
#' produce. A file holding no encoded blocks is returned unchanged, so this is
#' safe to call on any AudioMoth recording. Metadata chunks, including the GUANO
#' chunk written by current firmware, are carried over to the expanded file, and
#' are attached to a returned Wave object as a "guano" attribute, which
#' [guano()] will read.
#'
#' Writing to `output` streams the expanded file to disk, so a recording that
#' expands to more than memory can hold can still be expanded. Returning a Wave
#' object necessarily holds the whole expanded recording in memory.
#'
#' @param file Path to the T.WAV file to expand
#' @param output Path to write the expanded WAV file to. If NULL (the default)
#'   no file is written and a Wave object is returned instead.
#' @param overwrite If TRUE an existing output file is replaced
#' @return A Wave object, or, if `output` is given, the path to the written file
#'   (invisibly)
#' @export
#' @importFrom tuneR Wave
#' @references
#'   Open Acoustic Devices (2021) AudioMoth Triggered T.WAV File Format.
#'   \url{https://github.com/OpenAcousticDevices/Application-Notes}
#' @examples
#' \dontrun{
#' # Read a triggered recording into a Wave object
#' w <- audiomothExpand("20201103_112430T.WAV")
#'
#' # Expand every triggered recording on a card, as the reference tool does
#' for (f in list.files(pattern="T\\.WAV$")) {
#'   audiomothExpand(f, sub("T\\.WAV$", ".WAV", f))
#' }
#' }
#'
audiomothExpand <- function(file, output=NULL, overwrite=FALSE) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  if (!is.null(output) && file.exists(output) && !overwrite) {
    stop("Output file already exists, use overwrite=TRUE to replace it: ", output)
  }

  size <- file.size(file)
  nBlocks <- size %/% .AM_BLOCK_SIZE
  tailBytes <- size - nBlocks * .AM_BLOCK_SIZE

  # Read the whole blocks and the remainder separately, so that the blocks can
  # be given a matrix shape without copying the file a second time.
  con <- file(file, "rb")
  blocks <- readBin(con, "raw", n=nBlocks * .AM_BLOCK_SIZE)
  tail <- readBin(con, "raw", n=tailBytes)
  close(con)

  info <- tryCatch(
    .wavFormat(if (nBlocks > 0L) blocks else tail, total=size),
    unsupportedWav = function(cond) {
      # A header longer than the whole blocks read above does not appear in
      # them, so it is read again from the whole file. Only a file whose header
      # is unusually long reaches this, as the one AudioMoth writes is 488
      # bytes, but the application note warns that it may grow.
      if (tailBytes == 0L || nBlocks == 0L) {
        stop(cond)
      }
      .wavFormat(c(blocks, tail), total=size)
    }
  )
  if (info$format != 1) {
    .unsupportedWav("Only PCM files can be expanded.")
  }
  if (info$channels != 1) {
    .unsupportedWav("Only mono files can be expanded.")
  }
  if (info$bits != 16) {
    .unsupportedWav("Only 16-bit files can be expanded.")
  }
  if (info$dataTruncated) {
    warning("DATA chunk runs past the end of the file, which may be truncated.")
  }

  if (info$dataStart - 1 > nBlocks * .AM_BLOCK_SIZE) {
    # The header reaches past the last whole 512-byte block, so no block after
    # it could be an encoded block and there is nothing to expand. A recording
    # of a few samples is the usual way to arrive here.
    if (!is.null(output)) {
      # Asked to write over the file it was given, there is nothing to do:
      # the file already holds what expanding it would produce. Copying it
      # onto itself would fail, which a longer recording expanded in place
      # does not.
      same <- normalizePath(output, winslash="/", mustWork=FALSE) ==
        normalizePath(file, winslash="/", mustWork=FALSE)
      # file.copy() reports a failure, such as a directory that is not there,
      # by returning FALSE, so saying nothing here would hand back the path of
      # a file that was never written.
      if (!same && !file.copy(file, output, overwrite=TRUE)) {
        stop("Could not write the output file: ", output)
      }
      return(invisible(output))
    }
    return(.audiomothWave(c(blocks, tail), info, 0))
  }

  dim(blocks) <- c(.AM_BLOCK_SIZE, nBlocks)
  counts <- .audiomothBlockCounts(blocks)

  # Only the audio can hold an encoded block. A chunk written after it, such as
  # GUANO, is binary, and were some 512 bytes of it to read as an encoded block
  # it would be replaced by silence and counted into the audio.
  blockStart <- (seq_len(nBlocks) - 1) * .AM_BLOCK_SIZE + 1
  outside <- blockStart < info$dataStart |
    blockStart + .AM_BLOCK_SIZE - 1 > info$dataStart + info$dataSize - 1
  counts[outside] <- 0

  plan <- .audiomothExpandPlan(counts, tailBytes)
  outSize <- plan$total
  expanded <- outSize - size

  # A WAV file records its sizes in four bytes, so it cannot describe one of
  # 4GB or more. Writing such a file would wrap the sizes and leave something
  # no reader could make sense of. The sizes to weigh are the ones about to be
  # written, not the ones the file declares: a recording cut short can declare
  # a RIFF size far larger than anything it holds, and refusing it on that
  # would turn away a recording that expands perfectly well.
  if (outSize - 8 >= 2^32 || info$dataSize + expanded >= 2^32) {
    stop(
      "The expanded recording would be ", round(outSize / 2^30, 1),
      "GB, which is more than a WAV file can describe."
    )
  }

  # The chunk sizes still describe the unexpanded file. The RIFF size covers
  # every chunk, so it is taken from the length of the file written, which is
  # also right where the recording was cut short and the size it declared was
  # never true. The data size counts only the audio, so the bytes gained are
  # added to it rather than measured back from the end of the file, which keeps
  # any chunk written after the audio, such as GUANO, correct.
  blocks[5:8] <- .uintToRaw(outSize - 8, 4L)
  blocks[(info$dataStart - 4L):(info$dataStart - 1L)] <-
    .uintToRaw(info$dataSize + expanded, 4L)

  if (!is.null(output)) {
    # Streaming the expansion keeps a recording that expands to more than memory
    # can hold out of memory.
    out <- file(output, "wb")
    on.exit(close(out))
    for (step in seq_along(plan$source)) {
      switch(
        plan$source[step],
        blocks = writeBin(blocks[plan$from[step]:plan$to[step]], out),
        tail = writeBin(tail, out),
        blank = .writeZeros(out, plan$size[step])
      )
    }
    return(invisible(output))
  }

  if (outSize > .Machine$integer.max) {
    stop(
      "The expanded recording would be ", round(outSize / 2^30, 1),
      "GB, which is more than R can hold in one vector. Give an output file ",
      "to expand it to disk instead."
    )
  }

  # Omitted periods are silence, so only the bytes carried over have to be
  # written into the zeroed output.
  bytes <- raw(outSize)
  for (step in seq_along(plan$source)) {
    at <- plan$at[step]
    if (plan$source[step] == "blocks") {
      bytes[at:(at + plan$size[step] - 1)] <- blocks[plan$from[step]:plan$to[step]]
    } else if (plan$source[step] == "tail") {
      bytes[at:(at + plan$size[step] - 1)] <- tail
    }
  }

  return(.audiomothWave(bytes, info, expanded))
}

#' Build a Wave from the bytes of an expanded recording
#'
#' @param bytes A raw vector holding a whole WAV file
#' @param info Chunk positions from .wavFormat(), describing the file before it
#'   was expanded
#' @param expanded The number of bytes the expansion added
#' @return A Wave object, carrying any GUANO metadata as an attribute
#' @keywords internal
#' @noRd
.audiomothWave <- function(bytes, info, expanded) {
  dataEnd <- min(info$dataStart + info$dataSize + expanded - 1, length(bytes))
  samples <- readBin(
    bytes[info$dataStart:dataEnd], "integer",
    n=(dataEnd - info$dataStart + 1) %/% 2, size=2L, signed=TRUE, endian="little"
  )
  wave <- Wave(left=samples, samp.rate=info$sampleRate, bit=16, pcm=TRUE)

  metadata <- .guanoFromBytes(bytes)
  if (length(metadata) > 0L) {
    attr(wave, "guano") <- metadata
  }
  return(wave)
}

#' Build the 512-byte block encoding a skipped period
#'
#' @param nBlocks Length of the skipped period in 512-byte blocks
#' @return A raw vector of 512 bytes
#' @keywords internal
#' @noRd
.audiomothEncodedBlock <- function(nBlocks) {
  if (nBlocks < 1 || nBlocks > 2^32 - 1) {
    stop("A skipped period must be between 1 and 2^32-1 blocks long.")
  }
  bits <- floor(nBlocks / 2^(0:31)) %% 2
  samples <- integer(.AM_BLOCK_SIZE %/% 2L)
  samples[1:32] <- ifelse(bits == 1, 1L, -1L)
  return(writeBin(samples, raw(), size=2L, endian="little"))
}

#' Build an AudioMoth WAV header
#'
#' @param dataSize Size of the data chunk in bytes
#' @param sampleRate Samples per second
#' @param comment Comment string, stored in the ICMT chunk
#' @param artist Artist string, stored in the IART chunk
#' @param extraSize Size of any chunk written after the audio data
#' @return A raw vector of 488 bytes
#' @keywords internal
#' @noRd
.audiomothHeader <- function(dataSize, sampleRate, comment, artist, extraSize=0) {
  pad <- function(string, len) {
    bytes <- charToRaw(enc2utf8(as.character(string)))
    if (length(bytes) > len) {
      warning("String was truncated to ", len, " bytes to fit the WAV header.")
      bytes <- bytes[seq_len(len)]
    }
    return(c(bytes, raw(len - length(bytes))))
  }

  return(c(
    charToRaw("RIFF"), .uintToRaw(dataSize + extraSize + .AM_HEADER_SIZE - 8L, 4L),
    charToRaw("WAVE"),
    charToRaw("fmt "), .uintToRaw(16L, 4L),
    .uintToRaw(1L, 2L), .uintToRaw(1L, 2L),
    .uintToRaw(sampleRate, 4L), .uintToRaw(2 * sampleRate, 4L),
    .uintToRaw(2L, 2L), .uintToRaw(16L, 2L),
    charToRaw("LIST"), .uintToRaw(.AM_LIST_SIZE, 4L),
    charToRaw("INFO"),
    charToRaw("ICMT"), .uintToRaw(.AM_ICMT_SIZE, 4L), pad(comment, .AM_ICMT_SIZE),
    charToRaw("IART"), .uintToRaw(.AM_IART_SIZE, 4L), pad(artist, .AM_IART_SIZE),
    charToRaw("data"), .uintToRaw(dataSize, 4L)
  ))
}

#' Write a Wave as an AudioMoth triggered (T.WAV) file
#'
#' Writes a mono 16-bit Wave object in the AudioMoth triggered file format,
#' leaving out the periods that an amplitude threshold would not have triggered
#' a recording on. The wave is divided into segments of `segment` bytes, and a
#' segment whose peak amplitude is below `threshold` is omitted and replaced by
#' an encoded block recording its length. Omitted audio is not recoverable:
#' expanding the file gives silence in its place, as it would for a recording
#' the device had made with that threshold.
#'
#' The first and last segments are never omitted, as the first holds the file
#' header and the last may be shorter than a whole segment. Where these fall
#' below the threshold they are written as silence, which is what AudioMoth
#' firmware does.
#'
#' @param wave A mono 16-bit Wave object
#' @param file Path to write the T.WAV file to
#' @param threshold Peak amplitude, as a 16-bit sample value, at or above which
#'   a segment is kept. This is the amplitude threshold of an AudioMoth device.
#' @param segment Length of a segment in bytes. Must be a multiple of 512 and
#'   longer than the 488-byte header. The default of 32768 is what current
#'   AudioMoth firmware uses.
#' @param comment Comment string to store in the header, as read by
#'   [audiomothWave()]
#' @param artist Artist string to store in the header
#' @param guano GUANO metadata to store in the file, as a guano object from
#'   [guano()] or [guanoCreate()], or NULL for none
#' @param overwrite If TRUE an existing file is replaced
#' @return The path to the written file (invisibly)
#' @export
#' @references
#'   Open Acoustic Devices (2021) AudioMoth Triggered T.WAV File Format.
#'   \url{https://github.com/OpenAcousticDevices/Application-Notes}
#' @examples
#' \dontrun{
#' w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
#' audiomothCompress(w, "20201103_112430T.WAV", threshold=1000)
#' }
#'
audiomothCompress <- function(
  wave,
  file,
  threshold,
  segment=.AM_SEGMENT_SIZE,
  comment="",
  artist="",
  guano=NULL,
  overwrite=FALSE
) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a Wave object.")
  }
  if (wave@stereo) {
    stop("Only mono waves can be written as T.WAV files.")
  }
  if (wave@bit != 16) {
    stop("Only 16-bit waves can be written as T.WAV files.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0) {
    stop("threshold must be a single non-negative amplitude.")
  }
  if (segment %% .AM_BLOCK_SIZE != 0) {
    stop("segment must be a multiple of ", .AM_BLOCK_SIZE, " bytes.")
  }
  if (segment <= .AM_HEADER_SIZE) {
    stop("segment must be longer than the ", .AM_HEADER_SIZE, "-byte header.")
  }
  if (file.exists(file) && !overwrite) {
    stop("File already exists, use overwrite=TRUE to replace it: ", file)
  }

  samples <- wave@left
  n <- length(samples)
  if (n == 0L) {
    stop("wave contains no samples.")
  }
  if (any(samples < -32768 | samples > 32767)) {
    stop(
      "wave holds samples outside the range of a 16-bit recording, see ",
      "normalise()."
    )
  }

  # The header is written over the start of the first segment, so that every
  # later segment begins a multiple of 512 bytes into the file.
  firstSegment <- (segment - .AM_HEADER_SIZE) %/% 2L
  perSegment <- segment %/% 2L
  if (n <= firstSegment) {
    starts <- 1L
    ends <- n
  } else {
    starts <- c(1L, seq.int(firstSegment + 1L, n, by=perSegment))
    ends <- c(firstSegment, pmin(starts[-1L] + perSegment - 1L, n))
  }
  nSegments <- length(starts)

  triggered <- vapply(
    seq_len(nSegments),
    function(i) max(abs(samples[starts[i]:ends[i]])) >= threshold,
    logical(1)
  )
  written <- triggered
  written[1L] <- TRUE
  written[nSegments] <- TRUE

  parts <- vector("list", 2L * nSegments)
  used <- 0L
  skipped <- 0
  for (i in seq_len(nSegments)) {
    bytes <- (ends[i] - starts[i] + 1L) * 2L
    if (!written[i]) {
      skipped <- skipped + bytes
      next
    }
    if (skipped > 0) {
      used <- used + 1L
      parts[[used]] <- .audiomothEncodedBlock(skipped / .AM_BLOCK_SIZE)
      skipped <- 0
    }
    used <- used + 1L
    parts[[used]] <- if (triggered[i]) {
      writeBin(as.integer(samples[starts[i]:ends[i]]), raw(), size=2L, endian="little")
    } else {
      raw(bytes)
    }
  }

  parts <- parts[seq_len(used)]
  metadata <- if (is.null(guano)) raw(0) else .guanoChunk(guano)
  dataSize <- sum(vapply(parts, length, numeric(1)))

  con <- file(file, "wb")
  on.exit(close(con))
  writeBin(
    .audiomothHeader(dataSize, wave@samp.rate, comment, artist, length(metadata)),
    con
  )
  for (part in parts) {
    writeBin(part, con)
  }
  if (length(metadata) > 0L) {
    writeBin(metadata, con)
  }
  return(invisible(file))
}
