# Tests that draw would otherwise each open the default device, which writes an
# Rplots.pdf into the working directory. The test files run in parallel workers
# sharing that directory, so they corrupt one another's file and fail with
# "write failed" from plot.new(). A null device per worker keeps the drawing but
# writes nothing.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), testthat::teardown_env())
