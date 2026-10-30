# Source the pure-R parsing files of the package, without tuneR or seewave.
# Called by the other scripts in this directory.
local({
  root <- Sys.getenv("SONICSCREWDRIVER_ROOT", getwd())
  if (!dir.exists(file.path(root, "R"))) {
    stop("Run from the repository root, or set SONICSCREWDRIVER_ROOT.")
  }
  files <- c("wavChunks.R", "wavChunkMap.R", "metadataUtils.R", "guano.R",
             "bext.R", "id3.R", "xmp.R", "vorbisComment.R", "wamd.R",
             "wavMetadata.R", "olym.R", "readMetadata.R", "audiomoth.R",
             "parseFilename.R")
  for (f in files) {
    sys.source(file.path(root, "R", f), envir = globalenv())
  }
})
# Defined in files that need tuneR, and not reached by these scripts.
if (!exists(".validateChoice")) {
  .validateChoice <- function(x, choices, ...) {
    if (!x %in% choices) stop("bad choice")
    invisible(TRUE)
  }
}
if (!exists("annotation")) annotation <- function(...) list(...)
if (!exists("disp")) disp <- function(x) .mdObject(list(), "disp")
if (!exists(".mdXenoCantoSource")) .mdXenoCantoSource <- function(tag) NULL
