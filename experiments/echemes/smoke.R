sp <- "experiments/echemes"   #run from the repository root
pkgload::load_all(".", quiet = TRUE)
source(file.path(sp, "echemeDetector.R"))

mk <- function(onsets, len = 50) list(onsets = onsets, offsets = onsets + len)

cat("=== a song that groups: four pulses per echeme ===\n")
on <- as.vector(outer(c(0, 100, 200, 300), seq(0, 9)*2000, "+"))
r <- echemes(mk(sort(on)))
cat("  echemes", length(r$onsets), " pulses each", paste(unique(r$pulses), collapse=","),
    sprintf(" separation %.3f ratio %.1f threshold %.0f\n", r$separation, r$ratio, r$threshold))
cat("  first onset", r$onsets[1], "first offset", r$offsets[1],
    "(should be 0 and 350)\n")

cat("\n=== a continuous trill: one kind of silence ===\n")
r <- echemes(mk(seq(0, by = 100, length.out = 40)))
cat("  echemes", length(r$onsets), sprintf(" separation %.3f\n", r$separation))

cat("\n=== a trill with a little jitter ===\n")
set.seed(1)
on <- cumsum(c(0, round(stats::rnorm(39, 100, 8))))
r <- echemes(mk(on))
cat("  echemes", length(r$onsets), sprintf(" separation %.3f\n", r$separation))

cat("\n=== a trill with one accidental long gap ===\n")
on <- c(seq(0, by = 100, length.out = 20), seq(2500, by = 100, length.out = 20))
r <- echemes(mk(on))
cat("  echemes", length(r$onsets), sprintf(" separation %.3f ratio %s\n",
    r$separation, if (is.na(r$ratio)) "-" else sprintf("%.1f", r$ratio)))
cat("  (two runs of 20 is a real grouping, so echemes here are correct)\n")

cat("\n=== too few pulses ===\n")
r <- echemes(mk(c(0, 100, 200)))
cat("  echemes", length(r$onsets), " separation", r$separation, "\n")

cat("\n=== pulses touching, a zero distance ===\n")
on <- as.vector(outer(c(0, 50, 100, 150), seq(0, 9)*2000, "+"))
r <- echemes(mk(sort(on), len = 50))
cat("  echemes", length(r$onsets), " pulses each", paste(unique(r$pulses), collapse=","),
    sprintf(" separation %.3f\n", r$separation))

cat("\n=== refusals ===\n")
cat(" ", tryCatch(echemes(list(onsets=c(0,100))), error=function(e) conditionMessage(e)), "\n")
cat(" ", tryCatch(echemes(mk(c(0,100,200,300,400)), min.separation=2),
                  error=function(e) conditionMessage(e)), "\n")
cat(" ", tryCatch(echemes(mk(c(0,100,200,300,400)), min.pulses=0),
                  error=function(e) conditionMessage(e)), "\n")

cat("\n=== min.pulses rejects a split into single pulses ===\n")
# Mostly long silences with a few short ones: the division puts almost every
# pulse in an echeme of its own, which is a pulse and not a group of pulses.
gaps <- c(rep(60, 4), rep(1200, 25))
r <- echemes(mk(cumsum(c(0, gaps + 50))))
cat("  echemes", length(r$onsets), sprintf(" separation %.3f\n", r$separation))
cat("  with min.pulses=1 it would report",
    length(echemes(mk(cumsum(c(0, gaps + 50))), min.pulses=1)$onsets), "echemes\n")
