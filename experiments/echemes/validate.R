sp <- "experiments/echemes"   #run from the repository root
pkgload::load_all(".", quiet = TRUE)
source(file.path(sp, "echemeDetector.R"))
w <- readRDS(file.path(sp, "gryl_fetch.rds"))
w$path <- ifelse(!is.na(w$share), w$share, w$path)
M <- readRDS(file.path(sp, "gryl_temp_measured2.rds"))
M$good <- M$conc >= 0.95 & is.finite(M$snrDb) & M$snrDb >= 15
w <- w[w$id %in% M$id[M$good], ]

cleanWindow <- function(x, sr, frame.ms = 250, cap = 60) {
  n <- round(frame.ms*sr/1000); nf <- floor(length(x)/n)
  if (nf < 12) return(NULL)
  share <- numeric(nf)
  for (i in seq_len(nf)) {
    seg <- x[((i-1)*n+1):(i*n)]; m <- Mod(stats::fft(seg))^2
    bin <- seq_along(m) - 1; f <- pmin(bin, length(m) - bin)*sr/length(m)
    a <- sum(m[f >= 2000]); b <- sum(m[f >= 100 & f <= 1000])
    share[i] <- if (a + b == 0) 0 else a/(a + b)
  }
  r <- rle(share > 0.5); e <- cumsum(r$lengths); s <- e - r$lengths + 1
  g <- which(r$values); if (!length(g)) return(NULL)
  b <- g[which.max(r$lengths[g])]
  a <- (s[b] - 1)*n + 1; z <- min(e[b]*n, a + cap*sr - 1)
  if (z - a < 3*sr) NULL else c(a, z)
}

rows <- list()
for (i in seq_len(nrow(w))) {
  w0 <- tryCatch(tuneR::readWave(w$path[i]), error = function(e) NULL)
  if (is.null(w0)) next
  x0 <- as.numeric(w0@left); sr <- w0@samp.rate
  if (!length(x0) || anyNA(x0) || max(abs(x0), na.rm = TRUE) == 0) next
  x0 <- x0/max(abs(x0))
  cw <- cleanWindow(x0, sr); if (is.null(cw)) next
  ww <- tuneR::Wave(left = x0[cw[1]:cw[2]], samp.rate = sr, bit = 32, pcm = FALSE)
  band <- c(2000, min(20000, sr/2 - 1000))
  res <- tryCatch(pd_dietrich2004(ww, U = round(2.7e-3*sr), V = round(10.8e-3*sr),
                                  psi = max(1, round(sr/44100)), band = band),
                  error = function(e) NULL)
  if (is.null(res)) next
  k <- min(length(res$onsets), length(res$offsets)); if (k < 8) next
  e <- echemes(res)
  n <- length(e$onsets)
  rate <- if (n > 2) 1/(stats::median(diff(e$onsets))/sr) else NA_real_
  rows[[length(rows)+1]] <- data.frame(
    taxon = w$taxon[i], id = w$id[i], degC = w$degC[i], nPulse = k,
    nEch = n, echHz = rate,
    nsyl = if (n) stats::median(e$pulses) else NA_real_,
    separation = e$separation, ratio = e$ratio, stringsAsFactors = FALSE)
  cat(sprintf("%2d/%2d %-30s %-7s sep %.3f  %s\n", i, nrow(w),
      substr(w$taxon[i], 1, 30), w$id[i], e$separation,
      if (n) sprintf("%4d echemes, %5.1f pulses each, %.2f Hz", n,
                     stats::median(e$pulses), rate) else "no echemes"))
  flush.console()
}
V <- do.call(rbind, rows)
saveRDS(V, file.path(sp, "validate.rds"))

cat("\n=== by species ===\n")
print(do.call(rbind, lapply(split(V, V$taxon), function(g) {
  k <- g[is.finite(g$nsyl), ]
  data.frame(taxon = g$taxon[1], n = nrow(g), withEchemes = nrow(k),
             nsyl = if (nrow(k)) paste(sprintf("%.0f", sort(k$nsyl)), collapse=", ") else "-",
             spreadOct = if (nrow(k) > 1) round(log2(max(k$nsyl)/min(k$nsyl)), 2) else NA)
})), row.names = FALSE)
