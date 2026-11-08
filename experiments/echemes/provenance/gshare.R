sp <- "experiments/echemes"   #run from the repository root
SHARE <- "//valentine/mbl/share/workspaces/groups/urban-nature-project/audioblast/data/bio.acousti.ca"
w <- readRDS(file.path(sp, "gryl_fetch.rds"))

# Files on the share are named by the source's own id.
cand <- function(id) file.path(SHARE, paste0(id, c(".wav", ".WAV", ".mp3", ".flac")))
w$share <- vapply(w$id, function(id) {
  p <- cand(id); p <- p[file.exists(p)]
  if (length(p)) p[1] else NA_character_
}, character(1))
w$mb <- ifelse(is.na(w$share), NA, round(file.size(w$share)/1e6, 1))
cat("on the share:", sum(!is.na(w$share)), "of", nrow(w), "\n")
cat("total", round(sum(w$mb, na.rm = TRUE)), "MB\n\n")
print(do.call(rbind, lapply(split(w, w$taxon), function(g) data.frame(
  taxon = g$taxon[1], n = nrow(g), onShare = sum(!is.na(g$share)),
  degC = sprintf("%.0f-%.0f", min(g$degC), max(g$degC)),
  mb = round(sum(g$mb, na.rm = TRUE))))), row.names = FALSE)
miss <- w[is.na(w$share), c("taxon","id","duration")]
if (nrow(miss)) { cat("\nnot on the share:\n"); print(miss, row.names = FALSE) }
saveRDS(w, file.path(sp, "gryl_fetch.rds"))
