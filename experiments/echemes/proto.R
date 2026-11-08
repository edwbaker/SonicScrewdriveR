sp <- "experiments/echemes"   #run from the repository root
pkgload::load_all(".", quiet = TRUE)
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

# Split the pulse distances into two classes at the cut that maximises the
# variance between them, on the log scale because the two are separated by a
# ratio rather than a difference. This is Otsu's method in one dimension.
otsu <- function(v) {
  x <- sort(log(v))
  n <- length(x)
  if (n < 4) return(NULL)
  cs <- cumsum(x); tot <- cs[n]
  k <- seq_len(n - 1)
  m1 <- cs[k]/k                      # mean of the k smallest
  m2 <- (tot - cs[k])/(n - k)        # mean of the rest
  between <- (k/n)*((n - k)/n)*(m1 - m2)^2
  i <- which.max(between)
  list(thr = exp((x[i] + x[i + 1])/2),
       ratio = exp(m2[i] - m1[i]),
       eta2 = between[i]/stats::var(x)/((n - 1)/n),
       nLow = i, nHigh = n - i)
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
  on <- res$onsets[seq_len(k)]; off <- res$offsets[seq_len(k)]
  dist <- on[-1] - off[-k]           # the package's own pulse distance
  dist <- dist[dist > 0]
  o <- otsu(dist)
  if (is.null(o)) next
  grp <- cumsum(c(1, dist > o$thr))
  tab <- split(seq_along(grp), grp)
  nsyl <- vapply(tab, length, integer(1))
  starts <- on[vapply(tab, min, integer(1))]
  iv <- diff(starts)
  rows[[length(rows)+1]] <- data.frame(
    taxon = w$taxon[i], id = w$id[i],
    nPulse = k, ratio = o$ratio, eta2 = o$eta2,
    nEch = length(tab),
    echCV = if (length(iv) > 1 && mean(iv) > 0) stats::sd(iv)/mean(iv) else NA,
    nsylMed = stats::median(nsyl),
    nsylIQR = if (length(nsyl) > 2) stats::IQR(nsyl)/max(1, stats::median(nsyl)) else NA,
    stringsAsFactors = FALSE)
  cat(sprintf("%2d/%2d %-30s ratio %6.1f eta2 %.2f  nEch %4d  CV %5s  n/ech %5.1f\n",
      i, nrow(w), substr(w$taxon[i], 1, 30), o$ratio, o$eta2, length(tab),
      if (is.na(rows[[length(rows)]]$echCV)) "-" else sprintf("%.2f", rows[[length(rows)]]$echCV),
      stats::median(nsyl)))
}
P <- do.call(rbind, rows)
saveRDS(P, file.path(sp, "proto.rds"))
cat("\n=== by species ===\n")
print(do.call(rbind, lapply(split(P, P$taxon), function(g) data.frame(
  taxon = g$taxon[1], n = nrow(g),
  ratio = round(stats::median(g$ratio), 1),
  eta2 = round(stats::median(g$eta2), 2),
  echCV = round(stats::median(g$echCV, na.rm = TRUE), 2),
  nsylMed = round(stats::median(g$nsylMed), 1),
  nsylSpread = round(max(g$nsylMed)/max(1, min(g$nsylMed)), 1)))), row.names = FALSE)
