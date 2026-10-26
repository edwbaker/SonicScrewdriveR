# Builds the AudioMoth triggered (T.WAV) and GUANO fixtures in inst/extdata.
#
# Run from the root of the package with the package loaded:
#
#   devtools::load_all(); source("data-raw/audiomoth-fixtures.R")
#
# The triggered fixture is checked against the reference implementation
# published by Open Acoustic Devices, so that what is committed is known to
# expand correctly and the tests are not merely checking this package's reader
# against its own writer. To run that check, fetch and build the reference tool
# first, and give this script the path to it:
#
#   curl -o expand.c https://raw.githubusercontent.com/OpenAcousticDevices/\
#     Application-Notes/master/AudioMoth_Triggered_T_WAV_File_Format/expand.c
#   gcc -O2 -o expand expand.c
#
# Without it the fixture is still written, but the check is skipped and a
# message says so.

library(tuneR)

reference <- Sys.getenv("AUDIOMOTH_EXPAND", unset="")
extdata <- "inst/extdata"

# Everything here is deterministic arithmetic, with no use of the random number
# generator, so that the fixtures can be rebuilt identically on any version of
# R.
rate <- 8000L
segment <- 32768L
header <- 488L
first <- (segment - header) %/% 2L
per <- segment %/% 2L

# Four segments: a quiet first one, a quiet second one that is omitted, a
# triggered third one, and a short final one. The first and last segments are
# always written, so the file holds exactly one encoded block.
n <- first + 2L * per + 1000L
t <- seq_len(n) - 1L

# A quiet background that is not pure silence, so that the expanded result can
# be told apart from a file of zeros.
samples <- as.integer(round(40 * sin(2 * pi * 3 * t / rate)))

# One loud burst, wholly inside the third segment.
burst <- seq.int(first + per + 2000L, first + per + 2000L + 8000L)
samples[burst] <- as.integer(round(
  11000 * sin(2 * pi * 440 * (t[burst] - t[burst][1]) / rate)
))

twav <- file.path(extdata, "AUDIOMOTHT.WAV")
audiomothCompress(
  Wave(left=samples, samp.rate=rate, bit=16, pcm=TRUE),
  twav,
  threshold = 1000,
  segment = segment,
  overwrite = TRUE,
  comment = paste(
    "Recorded at 21:06:00 14/05/2026 (UTC) by AudioMoth 247AB4055C7B17DB",
    "at medium gain while battery was 4.1V and temperature was 12.3C.",
    "Amplitude threshold was 1000 with 1s minimum trigger duration."
  )
)

if (nzchar(reference)) {
  # The reference tool requires the T.WAV extension, and writes its output
  # alongside its input.
  work <- tempfile("audiomoth")
  dir.create(work)
  input <- file.path(work, "FIXTURET.WAV")
  file.copy(twav, input)
  stopifnot(identical(system2(reference, shQuote(input), stdout=TRUE), "OKAY"))

  ours <- file.path(work, "OURS.WAV")
  audiomothExpand(input, ours)
  theirs <- file.path(work, "FIXTURE.WAV")
  stopifnot(identical(
    readBin(theirs, "raw", n=file.size(theirs)),
    readBin(ours, "raw", n=file.size(ours))
  ))
  message("Expansion is identical to the reference implementation.")
} else {
  message(
    "Set AUDIOMOTH_EXPAND to the reference tool to check the fixture against it."
  )
}

# Metadata of the shape current AudioMoth firmware writes, including the fields
# it puts in its own namespace, on a short recording.
metadata <- guanoParse(paste(
  "GUANO|Version:1.0",
  "Make:Open Acoustic Devices",
  "Model:AudioMoth",
  "Serial:247AB4055C7B17DB",
  "Firmware Version:AudioMoth-Firmware-Basic (1.11.0)",
  "Timestamp:2026-05-14T21:06:00+01:00",
  "Loc Position:51.496400 -0.122400",
  "OAD|Loc Source:Configuration app",
  "Original Filename:20260514_210600.WAV",
  "OAD|Recording Settings:8000 GAIN 2 AMP 1000 1",
  "OAD|Battery Voltage:4.1",
  "Temperature Int:12.3",
  sep = "\n"
))

audiomothCompress(
  Wave(left=samples[1:800], samp.rate=rate, bit=16, pcm=TRUE),
  file.path(extdata, "AUDIOMOTH_GUANO.WAV"),
  threshold = 0,
  segment = segment,
  guano = metadata,
  overwrite = TRUE
)

message("Wrote ", twav, " and ", file.path(extdata, "AUDIOMOTH_GUANO.WAV"), ".")
