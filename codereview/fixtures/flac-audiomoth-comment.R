# R/readMetadata.R:827 - the AudioMoth comment parser runs for WAVE and MP3
# containers but not for FLAC or Ogg, so the same recording loses its datetime,
# make, model and serial when it is in a FLAC.
#
# Uses the package's own fixtures, which are the same recording in both forms.

source(file.path("codereview", "fixtures", "_load.R"))

E <- file.path("inst", "extdata")

for (nm in c("AUDIOMOTH.mp3", "AUDIOMOTH.flac")) {
  cat("####", nm, "####\n")
  md <- readMetadata(file.path(E, nm))
  cat("  container:", md$container,
      "  sources:", paste(names(md$sources), collapse = ", "), "\n")
  for (k in names(md$fields)) {
    cat(sprintf("  %-10s %s\n", k,
                substr(paste(format(md$fields[[k]]), collapse = " | "), 1, 80)))
  }
  cat("\n")
}

cat("#### the comment is in the FLAC's Vorbis fields all along ####\n")
v <- unclass(vorbisComment(file.path(E, "AUDIOMOTH.flac")))
for (k in c("COMMENT", "DESCRIPTION")) {
  cat(sprintf("  %-12s %s\n", k, substr(v[[k]], 1, 80)))
}

cat("\n#### and .audiomothComment() reads it perfectly well ####\n")
a <- .audiomothComment(c(v$COMMENT, v$DESCRIPTION))
cat("  start_date:", a$start_date, " start_time:", a$start_time,
    " time_zone:", a$time_zone, "\n")
cat("  serial    :", a$serial, " gain:", a$gain,
    " voltage:", a$voltage, " temperature:", a$temperature, "\n")
cat("\nreadMetadata() simply never calls it for a FLAC or Ogg container.\n")
