sp <- "experiments/echemes"   #run from the repository root
pkgload::load_all(".", quiet = TRUE)
w <- readRDS(file.path(sp, "gryl_fetch.rds"))
# Read straight off the audioblast share rather than the web host.
w$path <- ifelse(!is.na(w$share), w$share, w$path)
w <- w[file.exists(w$path) & file.size(w$path) > 1024, ]
w <- w[order(w$taxon != "Oecanthus pictus", w$duration), ]
cat("measuring", nrow(w), "recordings over", length(unique(w$taxon)), "species\n\n")

GAMMA <- 1.05; DG <- 0.2268; SIGMA <- 2
grid <- local({ t <- 0; repeat { n <- GAMMA*t[length(t)] + DG; if (n > 3000) break; t <- c(t, n) }; t })
dens <- function(d) vapply(grid, function(t) mean(exp(-(t - d)^2/(2*SIGMA^2))), numeric(1))

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
  if (is.null(w0)) { cat("read fail:", w$id[i], "\n"); next }
  x0 <- as.numeric(w0@left); sr <- w0@samp.rate
  if (!length(x0) || anyNA(x0) || max(abs(x0), na.rm = TRUE) == 0) next
  x0 <- x0/max(abs(x0))
  cw <- cleanWindow(x0, sr)
  if (is.null(cw)) { cat("no clean song:", w$id[i], w$taxon[i], "\n"); next }
  ww <- tuneR::Wave(left = x0[cw[1]:cw[2]], samp.rate = sr, bit = 32, pcm = FALSE)
  band <- c(2000, min(20000, sr/2 - 1000))
  U <- round(2.7e-3*sr); V <- round(10.8e-3*sr); psi <- max(1, round(sr/44100))
  res <- tryCatch(pd_dietrich2004(ww, U = U, V = V, psi = psi, band = band),
                  error = function(e) NULL)
  if (is.null(res)) { cat("detect fail:", w$id[i], "\n"); next }
  k <- min(length(res$onsets), length(res$offsets))
  if (k < 8) { cat("too few pulses:", w$id[i], w$taxon[i], k, "\n"); next }
  e <- res$e_u[res$e_u > 0]
  q <- if (length(e) >= 100) stats::quantile(e, c(0.05, 0.5, 0.95)) else c(NA, NA, NA)
  snrDb <- if (is.na(q[[1]]) || q[[1]] <= 0) NA else 10*log10(q[[3]]/q[[1]])
  on <- res$onsets[seq_len(k)]/sr; d <- diff(on)*1000
  D <- dens(d); m1 <- grid[which.max(D)]
  far <- grid > 2.5*m1
  m2 <- if (any(far) && max(D[far]) > 0) grid[far][which.max(D[far])] else NA
  thr <- if (is.na(m2)) NA else { b2 <- grid > m1 & grid < m2
    if (any(b2)) grid[b2][which.min(D[b2])] else 2.5*m1 }
  grp <- if (is.na(thr)) rep(1, k) else cumsum(c(1, d > thr))
  tab <- split(seq_len(k), grp); tab <- tab[vapply(tab, length, integer(1)) >= 2]
  echHz <- if (length(tab) > 2) {
    fs <- vapply(tab, function(z) on[min(z)], numeric(1)); 1/stats::median(diff(fs)) } else NA

  # Diagnostics of the echeme split itself. The interval density has to be
  # genuinely bimodal for echemes to exist: one mode for the gap inside an
  # echeme, a second for the gap between them, and a real valley between the
  # two. A continuous trill has only the first.
  valley <- NA_real_
  if (!is.na(m2) && !is.na(thr)) {
    b2 <- grid > m1 & grid < m2
    if (any(b2)) valley <- min(D[b2])/min(D[which.max(D)], max(D[far]))
  }
  nech <- length(tab)
  echCV <- NA_real_
  if (nech > 2) {
    fs <- vapply(tab, function(z) on[min(z)], numeric(1))
    iv <- diff(fs); if (length(iv) > 1 && stats::median(iv) > 0)
      echCV <- stats::sd(iv)/mean(iv)
  }
  nsylIQR <- if (length(tab)) {
    v <- vapply(tab, length, integer(1))
    if (length(v) > 2) stats::IQR(v)/max(1, stats::median(v)) else NA_real_ } else NA_real_
  m <- Mod(stats::fft(as.numeric(ww@left)/max(abs(ww@left))))^2
  bin <- seq_along(m) - 1; f <- pmin(bin, length(m) - bin)*sr/length(m)
  inB <- f >= band[1] & f <= band[2]
  om <- f[inB][which.max(m[inB])]
  rows[[length(rows)+1]] <- data.frame(
    taxon = w$taxon[i], id = w$id[i], degC = w$degC[i], sr = sr,
    secs = (cw[2] - cw[1] + 1)/sr, pulses = k, peak = om/1000,
    conc = sum(m[abs(f - om) < 500])/sum(m[f >= 2000]),
    snrDb = snrDb, sylHz = 1000/m1, echHz = echHz,
    sylPerEch = if (length(tab)) stats::median(vapply(tab, length, integer(1))) else NA,
    m1 = m1, m2 = m2, sep = m2/m1, valley = valley, nech = nech,
    echCV = echCV, nsylIQR = nsylIQR,
    stringsAsFactors = FALSE)
  cat(sprintf("%2d/%2d %-28s %-7s %4.1fC  syl %6.1f  ech %6s  n/ech %4s  conc %.2f  snr %4.1f\n",
      i, nrow(w), substr(w$taxon[i], 1, 28), w$id[i], w$degC[i], 1000/m1,
      if (is.na(echHz)) "-" else sprintf("%.2f", echHz),
      if (is.na(rows[[length(rows)]]$sylPerEch)) "-" else sprintf("%.0f", rows[[length(rows)]]$sylPerEch),
      rows[[length(rows)]]$conc, snrDb))
}
G <- do.call(rbind, rows)
saveRDS(G, file.path(sp, "gryl_temp_measured2.rds"))
cat("\nmeasured", nrow(G), "recordings over", length(unique(G$taxon)), "species\n")
