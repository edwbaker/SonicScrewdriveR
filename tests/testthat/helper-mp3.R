# The frames of an MPEG-1 Layer III stream, read from its bytes: where each
# begins, the main_data_begin of its side information, which says how many bytes
# before the frame its audio data begins, and whether it is the Xing or Info
# frame an encoder writes first, which holds no audio. Enough of the format for
# the constant bit rate files av::av_audio_convert() writes, no more.
mp3Frames <- function(bytes) {
  b <- as.integer(bytes)
  i <- 1
  if (b[1] == 0x49 && b[2] == 0x44 && b[3] == 0x33) {
    i <- 11 + sum(bitwAnd(b[7:10], 0x7f) * 128^(3:0))
  }
  rates <- c(0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320)
  samplingRates <- c(44100, 48000, 32000)
  tags <- lapply(c("Xing", "Info"), function(x) as.integer(charToRaw(x)))
  start <- integer(0)
  mdb <- integer(0)
  info <- logical(0)
  while (i + 40 <= length(b) && b[i] == 0xff && bitwAnd(b[i + 1], 0xe0) == 0xe0) {
    kbps <- rates[bitwShiftR(b[i + 2], 4) + 1]
    rate <- samplingRates[bitwAnd(bitwShiftR(b[i + 2], 2), 3) + 1]
    padding <- bitwAnd(bitwShiftR(b[i + 2], 1), 1)
    side <- i + 4 + if (bitwAnd(b[i + 1], 1) == 0) 2 else 0
    sideLength <- if (bitwShiftR(b[i + 3], 6) == 3) 17 else 32
    tag <- b[(side + sideLength):(side + sideLength + 3)]
    start <- c(start, i)
    mdb <- c(mdb, b[side] * 2 + bitwShiftR(b[side + 1], 7))
    info <- c(info, any(vapply(tags, identical, logical(1), tag)))
    i <- i + floor(144 * kbps * 1000 / rate) + padding
  }
  return(data.frame(start=start, mdb=mdb, info=info))
}
