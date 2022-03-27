setClass(
  "WaveFilter",
  slots=list(
    module="character",
    func="character",
    params="list"
  ),
  prototype = list(
    module = NA_character_,
    func = NA_character_,
    params = list()
  )
)

filterw <- function(w, filt) {
  if (filt@module == "seewave") {
    filt@params <- c(filt@params, "output"="Wave")
    return(do.call(match.fun(filt@func), c(w, filt@params)))
  }
}

bandpass <- function(...) {
  filt <- new("WaveFilter", module="seewave", func="ffilter", params=list(...))
  return(filt)
}
