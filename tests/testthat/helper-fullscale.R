# Mono wave files of any depth, in the RIFF form readWave() reads or the RF64
# form only the av package does, built from the bytes up so that the samples at
# full scale are exactly the ones asked for.

# Samples as the little-endian bytes of a data chunk: unsigned for 8 bits, as
# WAV holds them, two's complement above that, and IEEE floats for "float".
fsBytes <- function(x, bits) {
  if (identical(bits, "float")) {
    return(writeBin(as.numeric(x), raw(), size=4, endian="little"))
  }
  if (bits == 8) {
    return(as.raw(x + 128))
  }
  size <- bits / 8
  x <- as.numeric(x)
  x[x < 0] <- x[x < 0] + 256^size
  return(as.raw(unlist(lapply(x, function(v) floor(v / 256^(0:(size - 1))) %% 256))))
}

# A mono wave file of the samples, at the given depth, in either form.
fsWav <- function(x, bits, form="RIFF", rate=8000) {
  float <- identical(bits, "float")
  depth <- if (float) 32 else bits
  data <- fsBytes(x, bits)
  fmt <- mdChunk("fmt ", c(
    mdU16(if (float) 3 else 1), mdU16(1), mdU32(rate), mdU32(rate * depth / 8),
    mdU16(depth / 8), mdU16(depth)
  ))
  bytes <- if (form == "RIFF") {
    mdWav(fmt, mdChunk("data", data))
  } else {
    mdRf64(fmt, mdDeferred("data", data), dataSize=length(data), frames=length(x))
  }
  # Made here rather than by mdFile(), which ties the file to the frame that
  # calls it, so that it lasts as long as the test that asked for it.
  path <- withr::local_tempfile(fileext=".wav", .local_envir=parent.frame())
  writeBin(bytes, path)
  return(path)
}
