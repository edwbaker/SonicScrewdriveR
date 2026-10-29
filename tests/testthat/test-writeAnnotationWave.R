test_that("writeAnnotationWave works", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  a <- annotation(f, start = 0, end = 1)

  writeAnnotationWave(a)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  unlink("AUDIOMOTH_0-1.wav")

  b <- annotation(f, start = 1, end = 3)

  l <- list(a, b)

  writeAnnotationWave(l)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  expect_true(file.exists("AUDIOMOTH_1-3.wav"))
  unlink("AUDIOMOTH_0-1.wav")
  unlink("AUDIOMOTH_1-3.wav")


  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  wave <- readWave(f)
  a <- annotation(f, start = 0, end = 1)

  writeAnnotationWave(a, wave)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  unlink("AUDIOMOTH_0-1.wav")

  b <- annotation(f, start = 1, end = 3)

  l <- list(a, b)

  writeAnnotationWave(l, wave)
  expect_true(file.exists("AUDIOMOTH_0-1.wav"))
  expect_true(file.exists("AUDIOMOTH_1-3.wav"))
  unlink("AUDIOMOTH_0-1.wav")
  unlink("AUDIOMOTH_1-3.wav")
})

stereoWave <- function() {
  tuneR::Wave(left=round(sin(1:8000) * 2^22), right=round(cos(1:8000) * 2^22),
              samp.rate=8000, bit=24, pcm=TRUE)
}

test_that("every channel of a region is written, at the bit depth of the wave", {
  # seewave's cutw() kept the first channel alone.
  dir <- withr::local_tempdir()
  w <- stereoWave()
  out <- writeAnnotationWave(annotation("st.wav", start=0.25, end=0.5), w, dir=dir)
  expect_identical(out, file.path(dir, "st_0.25-0.5.wav"))
  got <- tuneR::readWave(out)
  expect_true(got@stereo)
  expect_identical(got@bit, 24L)
  expect_identical(got@left, w@left[2001:4000])
  expect_identical(got@right, w@right[2001:4000])
})

test_that("regions next to each other share no sample", {
  dir <- withr::local_tempdir()
  w <- stereoWave()
  out <- writeAnnotationWave(list(annotation("st.wav", start=0, end=0.5),
                                  annotation("st.wav", start=0.5, end=1)), w, dir=dir)
  first <- tuneR::readWave(out[1])
  second <- tuneR::readWave(out[2])
  expect_identical(c(first@left, second@left), w@left)
})

test_that("annotations of one region with different values are written to different files", {
  # Both were written to AUDIOMOTH_0-3.wav, so there was one file for the two.
  dir <- withr::local_tempdir()
  w <- stereoWave()
  out <- writeAnnotationWave(list(
    annotation("rec.wav", start=0, end=0.5, value="Turdus merula_Eurasian Blackbird"),
    annotation("rec.wav", start=0, end=0.5, value="Erithacus rubecula/Robin")
  ), w, dir=dir)
  expect_identical(basename(out), c("rec_0-0.5_Turdus_merula_Eurasian_Blackbird.wav",
                                    "rec_0-0.5_Erithacus_rubecula_Robin.wav"))
  expect_true(all(file.exists(out)))
})

test_that("an annotation running to the end of the file is written without remark", {
  dir <- withr::local_tempdir()
  expect_silent(out <- writeAnnotationWave(annotation("st.wav", start=0.5, end=Inf), stereoWave(), dir=dir))
  expect_identical(length(tuneR::readWave(out)@left), 4000L)
})

test_that("an annotation holding none of the wave writes nothing", {
  dir <- withr::local_tempdir()
  w <- stereoWave()
  expect_message(out <- writeAnnotationWave(annotation("st.wav", start=5, end=6), w, dir=dir),
                 "holds none of the wave")
  expect_identical(out, NA_character_)
  expect_message(out <- writeAnnotationWave(annotation("st.wav", start=NA, end=NA), w, dir=dir),
                 "holds none of the wave")
  expect_identical(out, NA_character_)
  expect_length(list.files(dir), 0)
})

test_that("a tagged wave is written as any other", {
  dir <- withr::local_tempdir()
  w <- tagWave(stereoWave())
  out <- writeAnnotationWave(annotation("st.wav", start=0, end=0.5), w, dir=dir)
  expect_identical(length(tuneR::readWave(out)@right), 4000L)
})
