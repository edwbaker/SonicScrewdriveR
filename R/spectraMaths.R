addSpectra <- function(s1, s2, coerceNegative="no") {
  validateComparableSpectra(s1, s2)
  if(coerceNegative == "input" | coerceNegative == "both") {
    coerceInput <- TRUE
  } else {
    coerceInput <- FALSE
  }
  s1 <- validateSpectrum(s1, coerceNegative=coerceInput)
  s2 <- validateSpectrum(s2, coerceNegative=coerceInput)
  s1[,2] <- s1[,2] + s2[,2]
  if(coerceNegative == "output" | coerceNegative == "both") {
    coerceOutput <- TRUE
  } else {
    coerceOutput <- FALSE
  }
  s1 <- validateSpectrum(s1, coerceNegative=coerceOutput)
  return(s1)
}

subtractSpectra <- function(s1, s2, coerceNegative="no") {
  validateComparableSpectra(s1, s2)
  if(coerceNegative == "input" | coerceNegative == "both") {
    coerceInput <- TRUE
  } else {
    coerceInput <- FALSE
  }
  s1 <- validateSpectrum(s1, coerceNegative=coerceInput)
  s2 <- validateSpectrum(s2, coerceNegative=coerceInput)
  s1[,2] <- s1[,2] - s2[,2]
  if(coerceNegative == "output" | coerceNegative == "both") {
    coerceOutput <- TRUE
  } else {
    coerceOutput <- FALSE
  }
  s1 <- validateSpectrum(s1, coerceNegative=coerceOutput)
  return(s1)
}

zeroSpectrum <- function(s1) {
  validateSpectrum(s1, coerceNegative = TRUE)
  s1[,2] <- rep_len(0, nrow(s1))
  return(s1)
}