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

test_that("binary units are converted", {
  expect_equal(convert2bytes(1, input="KiB"), 1024)
  expect_equal(convert2bytes(1, input="MiB"), 1024^2)
  expect_equal(convert2bytes(1, input="GiB"), 1024^3)
  expect_equal(convert2bytes(1, input="TiB"), 1024^4)
  expect_equal(convert2bytes(2, input="KiB"), 2048)
})

test_that("the larger decimal units are converted", {
  expect_equal(convert2bytes(1, input="TB"), 1e12)
  expect_equal(convert2bytes(1, input="PB"), 1e15)
  expect_equal(convert2bytes(1, input="EB"), 1e18)
})

test_that("fileSizeUnits lists the units", {
  expect_equal(fileSizeUnits(), c("kB", "MB", "GB", "TB", "PB", "EB"))
  expect_equal(fileSizeUnits("binary"), c("KiB", "MiB", "GiB", "TiB", "PiB", "EiB"))
  expect_error(fileSizeUnits("dog"), "Unknown units for file sizes")
})

test_that("humanBytes gives binary units", {
  expect_equal(humanBytes(1024, units="binary"), "1 KiB")
  expect_equal(humanBytes(1024^2, units="binary"), "1 MiB")
  expect_equal(humanBytes(1024^3, units="binary"), "1 GiB")
  expect_equal(humanBytes(1023, units="binary"), "1023 bytes")

  # The same size in each set of units.
  expect_equal(humanBytes(1048576), "1.049 MB")
  expect_equal(humanBytes(1048576, units="binary"), "1 MiB")

  expect_error(humanBytes(1, units="dog"), "Unknown units for file sizes")
})

test_that("humanBytes is vectorised", {
  # Previously this raised "the condition has length > 1".
  expect_equal(
    humanBytes(c(1, 2, 999, 1000, 1e6)),
    c("1 byte", "2 bytes", "999 bytes", "1 kB", "1 MB")
  )
  expect_equal(length(humanBytes(numeric(0))), 0)
})

test_that("humanBytes rounds, and can be asked not to", {
  expect_equal(humanBytes(1234567), "1.235 MB")
  expect_equal(humanBytes(1234567, digits=1), "1.2 MB")
  expect_equal(humanBytes(1234567, digits=NULL), "1.234567 MB")
})

test_that("humanBytes handles missing and zero values", {
  expect_equal(humanBytes(0), "0 bytes")
  expect_true(is.na(humanBytes(NA)))
  expect_equal(humanBytes(c(1000, NA)), c("1 kB", NA))
})
