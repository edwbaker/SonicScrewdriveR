maad_power2dB <- function(object, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }

  if (length(object)== 1) {
    ret <- maad$util$power2dB(object)
    return(ret)
  }

  ret <- maad$util$power2dB(reticulate::np_array(object))
  return(ret)
}
