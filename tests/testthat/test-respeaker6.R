# The Respeaker 6 writes eight channels, of which the last two carry no input
# audio, and readRespeaker6() keeps the six that do and names them. A recording
# of one sample frame is still a recording, and selecting channels from it must
# leave a matrix behind rather than a vector.

respeaker8 <- function(frames) {
  tuneR::WaveMC(
    data = matrix(rep(seq_len(8) * 100L, each=frames), nrow=frames),
    samp.rate = 16000, bit = 16
  )
}

test_that("A recording of one sample frame keeps its six channels", {
  f <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(respeaker8(1), f)

  w <- readRespeaker6(f)

  expect_identical(dim(w@.Data), c(1L, 6L))
  expect_identical(colnames(w@.Data), c("FL", "FR", "FC", "BL", "BR", "BC"))
  expect_identical(as.integer(w@.Data[1, ]), seq_len(6) * 100L)
})

test_that("A longer recording reads as it did", {
  f <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(respeaker8(50), f)

  w <- readRespeaker6(f)

  expect_identical(dim(w@.Data), c(50L, 6L))
  expect_identical(colnames(w@.Data), c("FL", "FR", "FC", "BL", "BR", "BC"))
})
