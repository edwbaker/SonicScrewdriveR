`%+%` <- function(a,b) {
  validateIsWave(a)
  validateIsWave(b)
  if (length(a@left) == length(b@left)) {
    return(a+b)
  }
  if (length(a@left) < length(b@left)) {
    a@left <- c(a@left + b@left[1:length(a@left)], b@left[(length(a@left)+1):length(b@left)])
    if (a@stereo && b@stereo) {
      a@right <- c(a@right + b@right[1:length(a@left)], b@right[(length(a@left)+1):length(b@left)])
    }
    return(a)
  }
  b@left <- c(b@left + a@left[1:length(b@left)], a@left[(length(b@left)+1):length(a@left)])
  if (b@stereo && a @stereo) {
    b@right <- c(b@right + a@right[1:length(b@left)], a@right[(length(b@left)+1):length(a@left)])
  }
  return(b)
}

`%+ovlp%` <- function(a,b) {
  validateIsWave(a)
  validateIsWave(b)
  if (length(a@left) < length(b@left)) {
    a@left <- a@left + b@left[1:length(a@left)]
    if (a@stereo && b@stereo) {
      a@right <- a@right + b@right[1:length(a@left)]
    }
    return(a)
  }
  b@left <- b@left + a@left[1:length(b@left)]
  if (b@stereo && a @stereo) {
    b@right <- b@right + a@right[1:length(b@left)]
  }
  return(b)
}

`%+cat%` <- function(a,b) {
  validateIsWave(a)
  validateIsWave(b)
  a@left <- c(a@left, b@left)
  if (a@stereo && b@stereo) {
   a@right <- c(a@right, b@right)
  }
  return(a)
}

`%+rep%` <- function(a,b) {
  validateIsWave(a)
  validateIsWave(b)
  if (length(a@left) < length(b@left)) {
    b@left <- b@left + rep_len(a@left, length(b@left))
    if (a@stereo && b@stereo) {
      b@right <- b@right + rep_len(a@right, length(b@left))
    }
    return(b)
  }
  a@left <- a@left + rep_len(b@left, length(a@left))
  if (a@stereo && b@stereo) {
    a@right <- a@right + rep_len(b@right, length(a@right))
  }
  return(a)
}
