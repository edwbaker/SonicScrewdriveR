test_that("upsample rejects incorrect input", {
  wave <- tuneR::sine(4000, samp.rate=44100)

  expect_error(upsample(wave, 44200, method="basic"),"Scale factor is not an integer.")

})

test_that("upsample works as expected", {
  wave <- tuneR::sine(4000, samp.rate=44100)

  upsampled_wave <- upsample(wave, wave@samp.rate*3, method="basic")
  expect_equal(length(wave@left)*3, length(upsampled_wave@left))

  expect_equal(wave@samp.rate*3, upsampled_wave@samp.rate)
  expect_equal(wave@bit, upsampled_wave@bit)
  expect_equal(wave@pcm, upsampled_wave@pcm)

  stereo <- stereo(wave, wave)
  upsampled_stereo <- upsample(stereo, stereo@samp.rate*5, method="basic")
  expect_equal(length(stereo@left)*5, length(upsampled_stereo@left))
  expect_equal(length(stereo@right)*5, length(upsampled_stereo@right))

  expect_equal(stereo@samp.rate*5, upsampled_stereo@samp.rate)
  expect_equal(stereo@bit, upsampled_stereo@bit)
  expect_equal(stereo@pcm, upsampled_stereo@pcm)
})
