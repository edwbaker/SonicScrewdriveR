sp <- "experiments/echemes"   #run from the repository root
source(file.path(sp, "getfun.R"))
R <- readRDS(file.path(sp, "gryl_recs.rds"))
b <- R[R$source == "bio.acousti.ca", ]
D <- get("details", type = "recordings", name = "temperature_start",
         id = paste(unique(b$id), collapse = ","), page_size = 100)
D$degC <- suppressWarnings(as.numeric(D$value))
b$degC <- D$degC[match(b$id, D$id)]

# Only the recordings whose temperature is known: they serve the repeatability
# test and the temperature response at once. Short ones first, so a run that is
# cut off still covers the most species.
w <- b[is.finite(b$degC) & b$duration <= 400, ]
w <- w[order(w$duration), ]
w$est_mb <- round(w$duration*w$sample_rate*2/1e6, 1)
w$path <- file.path(sp, "grylaudio",
                    paste0(gsub("[^A-Za-z0-9._-]", "_", paste0(w$source, "_", w$id)), ".wav"))
w$have <- file.exists(w$path) & file.size(w$path) > 1024
cat(nrow(w), "temperature bearing recordings,", sum(!w$have), "still to fetch,",
    round(sum(w$est_mb[!w$have])), "MB estimated\n\n")
print(do.call(rbind, lapply(split(w, w$taxon), function(g) data.frame(
  taxon = g$taxon[1], n = nrow(g), have = sum(g$have),
  degC = paste(range(g$degC), collapse = "-"),
  mb = round(sum(g$est_mb))))), row.names = FALSE)
saveRDS(w, file.path(sp, "gryl_fetch.rds"))
