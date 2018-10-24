convert2Pascals <- function(P, input="kPa") {
  if (input == "kPa") {
    return(P*1000)
  }
  if (input == "Pa") {
    return(P)
  }
}