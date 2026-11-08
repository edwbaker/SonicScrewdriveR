sp <- "experiments/echemes"   #run from the repository root
P <- readRDS(file.path(sp, "proto.rds"))
cat("recordings:", nrow(P), " species:", length(unique(P$taxon)), "\n\n")

cat("=== every recording, sorted by eta2 ===\n")
print(P[order(-P$eta2), c("taxon","id","nPulse","ratio","eta2","nEch","echCV","nsylMed")],
      row.names = FALSE, digits = 3)

# A species we can trust is one that gives the same answer from different
# recordings. That is the only ground truth available here, and it needs no
# catalogued value.
spread <- function(d) {
  v <- d$nsylMed[is.finite(d$nsylMed)]
  if (length(v) < 2) NA_real_ else log2(max(v)/max(1e-9, min(v)))
}

rules <- list(
  "none (what the old code did)"          = function(d) rep(TRUE, nrow(d)),
  "at least 2 pulses per echeme"          = function(d) d$nsylMed >= 2,
  "eta2 >= 0.8"                           = function(d) d$eta2 >= 0.8,
  "regular echemes, CV <= 0.5"            = function(d) is.finite(d$echCV) & d$echCV <= 0.5,
  "ratio >= 3"                            = function(d) d$ratio >= 3,
  "2 pulses + eta2 >= 0.8"                = function(d) d$nsylMed >= 2 & d$eta2 >= 0.8,
  "2 pulses + eta2 >= 0.8 + CV <= 0.5"    = function(d) d$nsylMed >= 2 & d$eta2 >= 0.8 &
                                                        is.finite(d$echCV) & d$echCV <= 0.5,
  "all four"                              = function(d) d$nsylMed >= 2 & d$eta2 >= 0.8 &
                                                        is.finite(d$echCV) & d$echCV <= 0.5 &
                                                        d$ratio >= 3
)
cat("\n=== how each rule behaves ===\n")
cat("   kept   = recordings accepted as having echemes\n")
cat("   spread = within species spread of syllables per echeme, in octaves (lower is better)\n\n")
for (nm in names(rules)) {
  keep <- rules[[nm]](P)
  keep[is.na(keep)] <- FALSE
  d <- P[keep, ]
  sps <- split(d, d$taxon)
  sp2 <- sps[vapply(sps, nrow, integer(1)) >= 2]
  sprd <- vapply(sp2, spread, numeric(1))
  cat(sprintf("  %-38s kept %2d/%2d  species %d  worst spread %s  median %s\n",
      nm, sum(keep), nrow(P), length(sps),
      if (!length(sprd)) "-" else sprintf("%.2f oct", max(sprd, na.rm = TRUE)),
      if (!length(sprd)) "-" else sprintf("%.2f", stats::median(sprd, na.rm = TRUE))))
  if (length(sps)) {
    for (g in sps) cat(sprintf("        %-30s n=%2d  n/ech %s\n",
        substr(g$taxon[1], 1, 30), nrow(g),
        paste(sprintf("%.0f", sort(g$nsylMed)), collapse = ", ")))
  }
}
