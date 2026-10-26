# Stops a write to a file part way through, so that a file is left partly
# written. Used to check that a write which fails does not take an existing
# recording with it.
#
# Only writes to a connection are stopped. These functions also use writeBin()
# to build bytes in memory, and stopping one of those would end the call before
# it ever opened a file, which would test nothing about what is left on disk.
#
# `after` says how many writes to the file to let through first, so that the
# file being written is left part written rather than empty.
interrupted <- function(expr, after = 1L) {
  assign(".interruptAfter", after, .GlobalEnv)
  trace(
    writeBin,
    tracer = quote({
      if (exists(".interruptAfter", .GlobalEnv) && !is.raw(con)) {
        left <- get(".interruptAfter", .GlobalEnv)
        if (left <= 0L) {
          stop("interrupted")
        }
        assign(".interruptAfter", left - 1L, .GlobalEnv)
      }
    }),
    print = FALSE
  )
  on.exit({
    if (exists(".interruptAfter", .GlobalEnv)) {
      rm(".interruptAfter", envir = .GlobalEnv)
    }
    untrace(writeBin)
  })
  tryCatch(expr, error = function(e) "interrupted")
}

# Was the write stopped after something had been written to the file, rather
# than before anything was? Returns the size of the part written file left
# behind by a stopped write, or NA where none was left.
partWritten <- function(directory) {
  parts <- list.files(directory, pattern = "[.]part$", full.names = TRUE)
  if (length(parts) == 0) {
    return(NA_integer_)
  }
  return(max(file.size(parts)))
}
