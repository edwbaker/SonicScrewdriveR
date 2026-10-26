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
#
# The stop raises a condition of its own, and only that condition is caught, so
# that a call which fails for some other reason, or which never writes to a
# file at all, is not reported as having been interrupted. Catching every error
# would let these tests pass whether or not the write they are about happened.
interrupted <- function(expr, after = 1L) {
  assign(".interruptAfter", after, .GlobalEnv)
  trace(
    writeBin,
    tracer = quote({
      if (exists(".interruptAfter", .GlobalEnv) && !is.raw(con)) {
        left <- get(".interruptAfter", .GlobalEnv)
        if (left <= 0L) {
          stop(structure(
            class = c("writeInterrupted", "error", "condition"),
            list(message = "the write was stopped for this test", call = NULL)
          ))
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
  tryCatch(expr, writeInterrupted = function(condition) "interrupted")
}
