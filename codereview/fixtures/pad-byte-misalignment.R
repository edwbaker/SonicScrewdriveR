# R/wavChunks.R:721 - .wavUnpadded() steps back over a legitimate pad byte.
#
# RIFF pads an odd-sized chunk body to an even length but does not specify the
# pad byte's value. These two files differ in that one byte and nothing else.

source(file.path("codereview", "fixtures", "_load.R"))

le <- function(x, n = 4L) .uintToRaw(x, n)

chunk <- function(id, body, pad = as.raw(0)) {
  out <- c(charToRaw(id), le(length(body)), body)
  if (length(body) %% 2 == 1) out <- c(out, pad)
  out
}

riff <- function(...) {
  payload <- c(charToRaw("WAVE"), ...)
  c(charToRaw("RIFF"), le(length(payload)), payload)
}

build <- function(padByte) {
  fmt <- c(le(1, 2), le(1, 2), le(48000, 4), le(96000, 4), le(2, 2), le(16, 2))

  # A bext chunk of ODD declared size: 602 fixed bytes plus one of coding
  # history. Real broadcast files reach an odd size the same way.
  bext <- raw(602)
  bext[1:4] <- charToRaw("Owl ")
  bext[321:330] <- charToRaw("2024-06-14")
  bext[331:338] <- charToRaw("12:34:56")
  bext <- c(bext, charToRaw("A"))
  stopifnot(length(bext) %% 2 == 1)

  info <- c(charToRaw("INFO"), charToRaw("ICMT"), le(244),
            charToRaw(strrep("c", 244)))

  riff(chunk("fmt ", fmt),
       chunk("bext", bext, pad = padByte),
       chunk("LIST", info),
       chunk("guan", charToRaw("GUANO|Version:1.0\nMake:Open Acoustic Devices")),
       chunk("data", as.raw(rep(1, 70000))))
}

for (pad in list(as.raw(0), charToRaw("X"))) {
  cat("\n#### pad byte after the odd-sized bext chunk = 0x",
      format(pad), " ####\n", sep = "")
  f <- tempfile(fileext = ".wav")
  writeBin(build(pad), f)
  cat("file size:", file.size(f), "\n")
  print(wavChunks(f))
  cat("guano()  :", paste(names(unclass(guano(f))), collapse = ", "), "\n")
  cat("wavInfo():", paste(names(unclass(wavInfo(f))), collapse = ", "), "\n")
  got <- try(.wavFormat(readBin(f, "raw", n = file.size(f))), silent = TRUE)
  cat(".wavFormat():",
      if (inherits(got, "try-error")) conditionMessage(attr(got, "condition"))
      else paste0("dataStart=", got$dataStart, " dataSize=", got$dataSize),
      "\n")
  unlink(f)
}

cat("\nBoth files are valid RIFF. The second loses its LIST, guan and data\n",
    "chunks, and .wavFormat() refuses it.\n", sep = "")
