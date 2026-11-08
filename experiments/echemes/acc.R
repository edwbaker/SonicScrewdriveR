sp <- "experiments/echemes"   #run from the repository root
V <- readRDS(file.path(sp, "validate.rds"))
T <- readRDS(file.path(sp, "gryl_traits.rds"))
P <- readRDS(file.path(sp, "proto.rds"))

cat("recordings reporting echemes:", sum(is.finite(V$nsyl)), "of", nrow(V), "\n\n")
M <- do.call(rbind, lapply(split(V[is.finite(V$nsyl), ], V$taxon[is.finite(V$nsyl)]),
  function(g) data.frame(taxon = g$taxon[1], n = nrow(g),
    ech = stats::median(g$echHz, na.rm = TRUE),
    nsyl = stats::median(g$nsyl), stringsAsFactors = FALSE)))

for (w in c("ech", "nsyl")) {
  lab <- if (w == "ech") "echeme rate (Hz)" else "syllables per echeme"
  cat("===", lab, "===\n")
  hits <- 0; tot <- 0; errs <- numeric(0)
  for (i in seq_len(nrow(M))) {
    s <- T[T$what == w & T$taxon == M$taxon[i], ]
    x <- M[[w]][i]
    if (!nrow(s) || !is.finite(x)) next
    hit <- any(x >= s$lo & x <= s$hi)
    near <- min(abs(log2(x/pmax(pmin(x, s$hi), s$lo))))
    hits <- hits + hit; tot <- tot + 1; errs <- c(errs, near)
    cat(sprintf("  %-30s n=%d  measured %6.2f  catalogued %-26s %s %.2f oct\n",
        substr(M$taxon[i], 1, 30), M$n[i], x,
        paste(sprintf("%.3g-%.3g", s$lo, s$hi), collapse = "; "),
        if (hit) "HIT " else "miss", near))
  }
  if (tot) cat(sprintf("  -> %d/%d in range, median %.2f oct from the nearest range\n\n",
                       hits, tot, stats::median(errs)))
}

cat("=== what the refusal cost and bought ===\n")
old <- P[is.finite(P$nsylMed), ]
sprd <- function(d) {
  s <- split(d, d$taxon); s <- s[vapply(s, nrow, integer(1)) >= 2]
  if (!length(s)) return(NA_real_)
  max(vapply(s, function(g) log2(max(g[[2]])/max(1e-9, min(g[[2]]))), numeric(1)))
}
cat(sprintf("  before: %2d/%2d recordings grouped, worst within species spread %.2f oct\n",
    nrow(old), nrow(P), sprd(data.frame(taxon = old$taxon, v = old$nsylMed))))
new <- V[is.finite(V$nsyl), ]
cat(sprintf("  after:  %2d/%2d recordings grouped, worst within species spread %.2f oct\n",
    nrow(new), nrow(V), sprd(data.frame(taxon = new$taxon, v = new$nsyl))))
cat("\n  Gryllus campestris by temperature (its spread is the only one left):\n")
g <- V[V$taxon == "Gryllus Gryllus campestris" & is.finite(V$nsyl), ]
print(g[order(g$degC), c("id","degC","nEch","echHz","nsyl","separation")],
      row.names = FALSE, digits = 3)
