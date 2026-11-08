# The detector is sourced over a loaded sonicscrewdriver so that it can use the
# package's own pulseStats() and validation helpers rather than keeping copies
# of them, and its tests are run the same way.
#
#   Rscript experiments/echemes/runTests.R
#
# Run it from the repository root.
sp <- "experiments/echemes"
pkgload::load_all(".", quiet = TRUE)
source(file.path(sp, "echemeDetector.R"))
testthat::test_file(file.path(sp, "test-echemeDetector.R"))
