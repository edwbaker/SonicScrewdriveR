maad_power2dB <- function(object, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }

  ret <- maad$util$power2dB(object)
  return(ret)
}
