# Stops the next write, so that a file is left part written. Used to check
# that a write which fails does not take an existing recording with it.
interrupted <- function(expr) {
  assign(".interruptWrite", TRUE, .GlobalEnv)
  trace(writeBin,
        tracer=quote(if (exists(".interruptWrite", .GlobalEnv)) stop("interrupted")),
        print=FALSE)
  on.exit({
    rm(".interruptWrite", envir=.GlobalEnv)
    untrace(writeBin)
  })
  tryCatch(expr, error=function(e) "interrupted")
}
