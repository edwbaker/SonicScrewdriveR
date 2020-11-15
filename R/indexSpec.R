#' @importFrom grDevices rgb
#' @importFrom seewave spectro
#' @importFrom tuneR normalize
indexSpec <- function(
    files,
    noise="/volumes/AAO/nhm-unp-1/_noise.wav",
    wl=256
){
  sf <-normalize(readWave(files[1]))
  s <- spectro(sf, f=sf@samp.rate, plot=FALSE, wl=wl)
  bins <- length(s$freq)

  nw <- readWave(noise)
  ns <- meanspec(nw, f=nw@samp.rate, plot=FALSE, wl=wl)

  aci <- entropy <- power <- matrix(, ncol=bins, nrow = length(files))


  for (i in 1:length(files)) {
    print(i)
    wave <- normalize(readWave(files[i]))
    spec <- spectro(wave, plot=FALSE, wl=wl)

    for (j in 1:bins) {
      spec$amp[,j] <- spec$amp[,j] - ns[,2]
    }

    power[i,] <- rowMeans(spec$amp^2)

    aci_t <- entropy_t <- vector(mode="numeric", length=bins)


    for (j in 1:bins) {
      aci_t[j] <- sum(abs(spec$amp[j,] - rev(spec$amp[j,])))/abs(sum(spec$amp[j,]))
      entropy_t[j] <- entropy(spec$amp[,j])
    }
    aci[i,] <- aci_t
    entropy[i,] <- entropy_t

  }

  power_s <- power/max(unlist(power))
  aci_s <- aci/max(unlist(aci))
  entropy_s <- entropy(max(unlist(entropy)))

  fcis_data <- rgb(power_s, aci_s, entropy_s)
  dim(fcis_data) <- dim (power_s)

  ret = list(power=power, aci=aci, entropy=entropy, fcis_data=t(fcis_data))
  return(ret)
}

