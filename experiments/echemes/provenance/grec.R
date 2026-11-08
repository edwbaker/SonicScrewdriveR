library(jsonlite)
sp <- "experiments/echemes"   #run from the repository root
source(file.path(sp, "getfun.R"))

S <- readRDS(file.path(sp, "gryl_species.rds"))
species <- sort(unique(c(S$echeme, S$syllable, S$perEch)))
cat(length(species), "Gryllidae species with timing traits\n\n")

rec <- list()
for (s in species) {
  d <- get("recordings", taxon=s, mime="audio/x-wav", page_size=100)
  n <- 0L; keep <- NULL
  if (!is.null(d) && nrow(d)) {
    d <- d[tolower(d$taxon) == tolower(s) &
           d$source %in% c("bio.acousti.ca", "TSA"), , drop=FALSE]
    if (nrow(d)) {
      d$sample_rate <- suppressWarnings(as.numeric(d$sample_rate))
      d$duration <- suppressWarnings(as.numeric(d$duration))
      d <- d[!is.na(d$sample_rate) & d$sample_rate >= 22050 &
             !is.na(d$duration) & d$duration >= 10, , drop=FALSE]
      if (nrow(d)) {
        n <- nrow(d)
        keep <- d[order(-d$duration), c("source","id","taxon","filename",
                                        "license","sample_rate","duration")]
      }
    }
  }
  cat(sprintf("  %-34s %3d usable\n", substr(s, 1, 34), n))
  if (n) rec[[s]] <- keep
}
R <- do.call(rbind, rec)
cat("\nspecies with >=1 usable recording:", length(rec), "\n")
cat("species with >=2 (repeatability possible):",
    sum(vapply(rec, nrow, integer(1)) >= 2), "\n")
cat("species with >=3:", sum(vapply(rec, nrow, integer(1)) >= 3), "\n")
cat("total usable recordings:", nrow(R), "\n")
cat("\nby source:\n"); print(table(R$source))
cat("\nsample rates:\n"); print(table(R$sample_rate))
cat("\nduration (s): "); print(summary(R$duration))
cat("\nlicence known for", sum(!is.na(R$license) & nzchar(R$license)), "of", nrow(R), "\n")
saveRDS(R, file.path(sp, "gryl_recs.rds"))
