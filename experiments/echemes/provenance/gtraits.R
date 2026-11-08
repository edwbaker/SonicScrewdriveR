sp <- "experiments/echemes"   #run from the repository root
G <- readRDS(file.path(sp, "gryl_traits_raw.rds"))

# Numeric range from a trait value written in any of the archive's forms.
rng <- function(value, vmin, vmax) {
  v <- trimws(as.character(value))
  lo <- suppressWarnings(as.numeric(vmin)); hi <- suppressWarnings(as.numeric(vmax))
  if (!is.na(lo) && !is.na(hi)) return(c(lo, hi))
  v2 <- gsub("\u2013|\u2014", "-", v)
  if (grepl("^<", v2)) { x <- suppressWarnings(as.numeric(sub("^<", "", v2)))
    return(if (is.na(x)) c(NA, NA) else c(0, x)) }
  if (grepl("^>", v2)) { x <- suppressWarnings(as.numeric(sub("^>", "", v2)))
    return(if (is.na(x)) c(NA, NA) else c(x, Inf)) }
  if (grepl("\u00b1", v2)) {
    p <- strsplit(v2, "\u00b1")[[1]]
    a <- suppressWarnings(as.numeric(p[1])); b <- suppressWarnings(as.numeric(p[2]))
    return(if (any(is.na(c(a, b)))) c(NA, NA) else c(a - b, a + b))
  }
  if (grepl("^[0-9.]+-[0-9.]+$", v2)) return(range(as.numeric(strsplit(v2, "-")[[1]])))
  x <- suppressWarnings(as.numeric(v2))
  if (is.na(x)) c(NA, NA) else c(x, x)
}

# Calling song only; the archive writes it two ways, with section/position qualifiers.
G <- G[grepl("^Calling (Call|Song)", G$callType), , drop = FALSE]

RATE <- c("Echeme Repetition Rate (Hz)" = "ech",
          "Syllable Repetition Rate (in echeme) (Hz)" = "syl",
          "Syllable Repetition Rate (Continuous Song) (Hz)" = "syl",
          "Syllable Repetition Rate (Repeated Syllables in Song)" = "syl")
PERIOD <- c("Echeme Period" = "ech", "Syllable Period" = "syl",
            "Isolated Syllable Period" = "syl")
COUNT <- c("Syllables per Echeme" = "nsyl")

out <- list()
for (i in seq_len(nrow(G))) {
  tr <- G$trait[i]
  r <- rng(G$value[i], G$min[i], G$max[i])
  if (any(is.na(r))) next
  if (tr %in% names(RATE)) {
    what <- RATE[[tr]]; lo <- r[1]; hi <- r[2]
  } else if (tr %in% names(PERIOD)) {
    if (r[1] <= 0) next                      # a period of zero gives no rate
    what <- PERIOD[[tr]]; lo <- 1/r[2]; hi <- 1/r[1]   # inverting swaps the ends
  } else if (tr %in% names(COUNT)) {
    what <- COUNT[[tr]]; lo <- r[1]; hi <- r[2]
  } else next
  out[[length(out) + 1]] <- data.frame(taxon = G$taxon[i], what = what, lo = lo, hi = hi,
                                       trait = tr, raw = G$value[i],
                                       stringsAsFactors = FALSE)
}
T <- do.call(rbind, out)
T <- T[is.finite(T$lo), ]
cat("usable Gryllidae trait ranges:", nrow(T), "over", length(unique(T$taxon)), "species\n\n")
for (w in c("syl", "ech", "nsyl")) {
  k <- T[T$what == w, ]
  cat(sprintf("%-5s %3d values, %2d species, span %.3g to %.3g\n",
      w, nrow(k), length(unique(k$taxon)),
      min(k$lo), max(k$hi[is.finite(k$hi)])))
}
cat("\nper species:\n")
agg <- do.call(rbind, lapply(split(T, T$taxon), function(g) data.frame(
  taxon = g$taxon[1],
  syl = sum(g$what == "syl"), ech = sum(g$what == "ech"), nsyl = sum(g$what == "nsyl"))))
print(agg[order(-(agg$syl > 0) - (agg$ech > 0) - (agg$nsyl > 0), agg$taxon), ], row.names = FALSE)
saveRDS(T, file.path(sp, "gryl_traits.rds"))
