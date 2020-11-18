rainfall_hardRain <- function(wave, threshold="default") {
  if (threshold == "default") {
    train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)
    threshold <- hardRain::getThreshold(train.fn, fn = "spec")
  }
  ret <- hardRain::classifyRain(wave, threshold)
}
