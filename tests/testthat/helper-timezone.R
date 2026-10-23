# Functions that read a time are expected to give the same answer whatever
# timezone the machine running the analysis is set to, so the tests that involve
# a time are repeated in several session timezones rather than pinning the
# session to UTC, which is the condition under which such a bug does not show.
sessionTimezones <- c("UTC", "America/New_York", "Australia/Sydney")

withSessionTimezone <- function(tz, code) {
  old <- Sys.getenv("TZ", unset=NA)
  Sys.setenv(TZ=tz)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ=old)
  })
  force(code)
}

# Comparing the epoch seconds rather than the POSIXct objects themselves, and
# with tolerance=0. The tolerance expect_equal() allows is relative, which for a
# present-day datetime comes to about 25 seconds, so a comparison left at the
# default would pass whether or not the seconds are right.
expect_instant <- function(parsed, epoch) {
  expect_equal(as.numeric(parsed$datetime), as.numeric(epoch), tolerance=0)
}

# A POSIXct holding an instant but no timezone, as Sys.time() and as.POSIXct()
# on a plain number both return. How these read used to depend on the session.
naiveTime <- function(epoch) {
  structure(as.numeric(epoch), class=c("POSIXct", "POSIXt"))
}
