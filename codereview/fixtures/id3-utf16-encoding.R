# R/id3.R:45 - the UTF-16 byte order of a frame's value is inferred from the
# bytes at the start of its DESCRIPTION, so a conforming value carrying its own
# byte order mark is decoded with the wrong order.

source(file.path("codereview", "fixtures", "_load.R"))

syncsafe <- function(x) {
  d <- integer(4)
  for (i in 4:1) { d[i] <- x %% 128; x <- x %/% 128 }
  as.raw(d)
}

frame24 <- function(id, body) {
  c(charToRaw(id), syncsafe(length(body)), as.raw(c(0, 0)), body)
}

tag24 <- function(frames) {
  c(charToRaw("ID3"), as.raw(c(4, 0, 0)), syncsafe(length(frames)), frames)
}

# UTF-16LE, optionally with the BOM the ID3v2 specification requires.
utf16le <- function(s, bom = TRUE) {
  cp <- utf8ToInt(s)
  c(if (bom) as.raw(c(0xff, 0xfe)) else raw(0),
    as.raw(as.vector(rbind(cp %% 256, cp %/% 256))))
}

cat("#### what .id3Encoding() makes of the leading bytes ####\n")
for (case in list(
  list("LE BOM", as.raw(c(0xff, 0xfe, 0x64, 0x00))),
  list("BE BOM", as.raw(c(0xfe, 0xff, 0x00, 0x64))),
  list("no BOM", as.raw(c(0x64, 0x00))),
  list("bare 00 00 terminator", as.raw(c(0x00, 0x00)))
)) {
  cat(sprintf("  code 1, %-22s -> %s\n", case[[1]],
              .id3Encoding(1L, case[[2]])))
}

cat("\n#### COMM, encoding 1, EMPTY description, conforming UTF-16LE value ####\n")
# encoding byte, language "eng", empty description written as the bare two-byte
# terminator, then the value with its own little-endian BOM.
body <- c(as.raw(1), charToRaw("eng"), as.raw(c(0, 0)), utf16le("dusk"))
got <- unclass(.id3Parse(tag24(frame24("COMM", body))))$COMM
cat("  got :", got, "\n")
cat("  want: dusk\n")
cat("  The value is well formed; only the description lacks a BOM.\n")

cat("\n#### TIT2, encoding 1, UTF-16LE with no BOM at all ####\n")
got <- unclass(.id3Parse(tag24(frame24(
  "TIT2", c(as.raw(1), utf16le("dusk", bom = FALSE))))))$TIT2
cat("  got :", got, "\n")
cat("  want: dusk\n")
cat("  Non-conforming input, but UTF-16LE is the order taggers omit the BOM\n",
    "  from; the fallback picks UTF-16BE.\n", sep = "")

cat("\n#### for contrast: the same frames written as the specification asks ####\n")
cat("  COMM, description with its own BOM:",
    unclass(.id3Parse(tag24(frame24("COMM", c(
      as.raw(1), charToRaw("eng"), utf16le(""), as.raw(c(0, 0)),
      utf16le("dusk"))))))$COMM, "\n")
cat("  TIT2, value with its own BOM      :",
    unclass(.id3Parse(tag24(frame24("TIT2", c(
      as.raw(1), utf16le("dusk"))))))$TIT2, "\n")
