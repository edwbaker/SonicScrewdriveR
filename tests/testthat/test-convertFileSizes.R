test_that("Reject invalid input", {
  expect_error(convert2bytes(1, "dog"))
})

test_that("Conversion is correct", {
  expect_equal(convert2bytes(8, input="bits"), 1)
  expect_equal(convert2bytes(1, input="bytes"), 1)
  expect_equal(convert2bytes(1, input="kB"), 1000)
  expect_equal(convert2bytes(1, input="MB"), 1000000)
  expect_equal(convert2bytes(1, input="GB"), 1000000000)
})

test_that("humanBytes is correct", {
  expect_equal(humanBytes(1), "1 byte")
  expect_equal(humanBytes(2), "2 bytes")
  expect_equal(humanBytes(999), "999 bytes")
  expect_equal(humanBytes(1000), "1 kB")
  expect_equal(humanBytes(999999), "999.999 kB")
  expect_equal(humanBytes(1000000), "1 MB")
  expect_equal(humanBytes(1000000000), "1 GB")
  expect_equal(humanBytes(1000000000000), "1 TB")
  expect_equal(humanBytes(1000000000000000), "1 PB")
  expect_equal(humanBytes(1000000000000000000), "1 EB")
})
