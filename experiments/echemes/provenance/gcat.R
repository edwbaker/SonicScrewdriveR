library(jsonlite)
sp <- "experiments/echemes"   #run from the repository root

get <- function(path, ...) {
  q <- list(...)
  q <- q[!vapply(q, is.null, logical(1))]
  u <- paste0("https://api.audioblast.org/data/", path, "/?",
              paste(sprintf("%s=%s", names(q), vapply(q, function(v)
                utils::URLencode(as.character(v), reserved=TRUE), character(1))),
                collapse="&"))
  out <- list(); page <- 1
  repeat {
    r <- tryCatch(fromJSON(paste0(u, "&page=", page)), error=function(e) NULL)
    if (is.null(r) || !is.data.frame(r$data) || nrow(r$data) == 0) break
    out[[length(out)+1]] <- r$data
    if (is.null(r$last_page) || page >= r$last_page) break
    page <- page + 1
  }
  if (!length(out)) return(NULL)
  do.call(rbind, lapply(out, function(d) d[, Reduce(intersect, lapply(out, names)), drop=FALSE]))
}

TRAITS <- c("Echeme Repetition Rate (Hz)", "Echeme Period", "Echeme Interval",
            "Syllable Repetition Rate (in echeme) (Hz)",
            "Syllable Repetition Rate (Continuous Song) (Hz)",
            "Syllable Repetition Rate (Repeated Syllables in Song)",
            "Syllable Period", "Isolated Syllable Period", "Syllable Interval",
            "Syllables per Echeme")

all <- list()
for (t in TRAITS) {
  d <- get("traitstaxa", family="Gryllidae", trait=t, page_size=100)
  if (is.null(d) || !nrow(d)) { cat(sprintf("%-55s  none\n", t)); next }
  d <- d[!duplicated(d$traitID), , drop=FALSE]
  cat(sprintf("%-55s  %3d values, %3d species   e.g. %s\n", t, nrow(d),
      length(unique(d$taxon)), paste(utils::head(unique(d$value), 4), collapse=", ")))
  all[[t]] <- d[, c("traitID","taxon","trait","value","min","max","callType","sex")]
}
G <- do.call(rbind, all)
cat("\ntotal", nrow(G), "trait values over", length(unique(G$taxon)), "Gryllidae species\n")
cat("callType:\n"); print(table(G$callType, useNA="ifany"))
saveRDS(G, file.path(sp, "gryl_traits_raw.rds"))

cat("\n=== species with any echeme-level and any syllable-level trait ===\n")
echT <- c("Echeme Repetition Rate (Hz)", "Echeme Period", "Echeme Interval")
sylT <- c("Syllable Repetition Rate (in echeme) (Hz)",
          "Syllable Repetition Rate (Continuous Song) (Hz)",
          "Syllable Repetition Rate (Repeated Syllables in Song)",
          "Syllable Period", "Isolated Syllable Period", "Syllable Interval")
spE <- unique(G$taxon[G$trait %in% echT])
spS <- unique(G$taxon[G$trait %in% sylT])
spN <- unique(G$taxon[G$trait == "Syllables per Echeme"])
cat("  echeme-level:", length(spE), " syllable-level:", length(spS),
    " syllables-per-echeme:", length(spN), "\n")
cat("  union:", length(unique(c(spE, spS, spN))), "species\n")
saveRDS(list(echeme=spE, syllable=spS, perEch=spN), file.path(sp, "gryl_species.rds"))
