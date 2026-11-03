.recWave <- function(samples=4410) {
  tuneR::sine(440, duration=samples, samp.rate=44100)
}

test_that("a record keeps the defaults the function chose", {
  tw <- tagWave(.recWave())
  cutoff <- function(x, cutoff=2000, order=4) x

  # A record said "cutoff" and nothing else, though two of the three things
  # that decided what happened were the function's own.
  rec <- tagApply(tw, cutoff, label="demo")@processing[[1]]
  expect_equal(rec$params, list())
  expect_equal(rec$defaults, list(cutoff="2000", order="4"))

  # What the caller chose leaves the defaults and is never repeated in them.
  rec <- tagApply(tw, cutoff, order=8)@processing[[1]]
  expect_equal(rec$params, list(order=8))
  expect_equal(rec$defaults, list(cutoff="2000"))

  # A function with nothing left to a default has no such field, which is how
  # every function of this package records: they pass their arguments on in
  # full, so .cutws(wave, from, to, plot) has no default to find.
  expect_null(tagApply(tw, function(x, y) x, y=1)@processing[[1]]$defaults)
  expect_null(cutws(tw, from=0, to=100)@processing[[1]]$defaults)
  expect_null(normalise(tw)@processing[[1]]$defaults)
})

test_that("the defaults of a filter say how it filtered", {
  # The case this was for: a record of an ffilter call named the band and not
  # the window, though the window length, overlap and shape change the samples
  # that come back.
  tw <- tagWave(noise("white", duration=4410, samp.rate=44100))
  rec <- tagApply(
    tw, seewave::ffilter, from=1000, to=2000, output="Wave"
  )@processing[[1]]

  expect_equal(rec$params, list(from=1000, to=2000, output="Wave"))
  expect_equal(rec$defaults$wl, "1024")
  expect_equal(rec$defaults$ovlp, "75")
  expect_equal(rec$defaults$wn, "\"hanning\"")
  # f has no default, so there is none to record for it.
  expect_false("f" %in% names(rec$defaults))
})

test_that("a default is recorded as written, not as evaluated", {
  tw <- tagWave(.recWave())

  # tuneR::normalize defaults pcm to a slot of its own argument, which has no
  # value outside the call. Recording the expression says what the rule was;
  # evaluating it here would either fail or answer for a different object.
  rec <- tagApply(tw, tuneR::normalize, unit="1")@processing[[1]]
  expect_equal(rec$defaults$pcm, "object@pcm")
  # A default that is a literal is its own value.
  expect_equal(rec$defaults$center, "TRUE")
  expect_equal(rec$defaults$level, "1")

  # A default too long to keep whole is cut rather than filling the record.
  expect_equal(.shortDeparse(quote(x + 1)), "x + 1")
  capped <- .shortDeparse(as.character(seq_len(200)))
  expect_equal(nchar(capped), 120)
  expect_match(capped, "[.][.][.]$")
  long <- function(x, table=c("one", "two", "three", "four", "five", "six",
                              "seven", "eight", "nine", "ten", "eleven",
                              "twelve", "thirteen", "fourteen")) x
  recorded <- tagApply(tw, long)@processing[[1]]$defaults$table
  expect_equal(nchar(recorded), 120)
})

test_that("an argument given by position or in part is named as the formal", {
  tw <- tagWave(.recWave())
  cutoff <- function(x, cutoff=2000, order=4) x

  # A record of list(3000) did not say which setting 3000 was.
  expect_equal(tagApply(tw, cutoff, 3000)@processing[[1]]$params, list(cutoff=3000))
  # R matches an abbreviation to the formal, so a record should name the formal.
  expect_equal(tagApply(tw, cutoff, ord=8)@processing[[1]]$params, list(order=8))
  # Several at once, in the order the function takes them rather than the order
  # they were written.
  expect_equal(
    tagApply(tw, cutoff, order=8, 3000)@processing[[1]]$params,
    list(cutoff=3000, order=8)
  )
})

test_that("a function that names none of its arguments is recorded as given", {
  tw <- tagWave(.recWave())
  # Everything reaches `...`, so there is no formal to name an argument after
  # and nothing is left to a default.
  rec <- tagApply(tw, function(...) list(...)[[1]], 3000, "x")@processing[[1]]
  expect_equal(rec$params, list(3000, "x"))
  expect_null(rec$defaults)
})

test_that("a call that cannot be matched keeps the names it was given", {
  # Not reachable through tagApply(), as a function that would refuse the call
  # raises its own error first. The fallback is here so that a record is never
  # lost to a function that matches its arguments in some way of its own.
  expect_null(.matchedCall(function(x, y) x, list(nosuch=1)))
  expect_equal(
    .describeCall(function(x, y) x, list(nosuch=1)),
    list(params=list(nosuch=1), defaults=NULL)
  )
})

test_that("a formal the wave filled is not recorded as a default", {
  tw <- tagWave(.recWave())
  # R gives a named argument its formal first, so the wave lands on whichever
  # is left: here it is y, which was therefore never left to its default. A
  # record that assumed the wave took the first formal called y a default of 2
  # while y was the whole recording.
  rec <- tagApply(tw, function(x, y=2) y, x=5)@processing[[1]]
  expect_equal(rec$params, list(x=5))
  expect_null(rec$defaults)
  expect_equal(rec$input_hash, rec$output_hash)
})

test_that("a function with no formals to match is described as called", {
  tw <- tagWave(.recWave())
  # `(` is a function whose arguments are not matched against formals at all.
  rec <- tagApply(tw, `(`)@processing[[1]]
  expect_equal(rec$func, "(")
  expect_equal(rec$params, list())
  expect_null(rec$defaults)
  # It gives the wave back, so the digests either side agree.
  expect_equal(rec$input_hash, rec$output_hash)

  expect_equal(.describeCall(`(`, list(1)), list(params=list(1), defaults=NULL))
  # A function with no formals has no defaults either.
  expect_null(.argDefaults(function() NULL, quote(fun(..wave))))
})

test_that("a record says which zone its time was taken in", {
  tw <- tagWave(.recWave())
  rec <- tagApply(tw, tuneR::normalize, unit="1")@processing[[1]]

  # A POSIXct alone is shown in the zone of whoever reads it, and loses the
  # zone altogether once written out as text.
  expect_s3_class(rec$time, "POSIXct")
  expect_true(is.character(rec$timezone) && nzchar(rec$timezone))

  # A machine that cannot name its zone records the abbreviation instead, which
  # is covered below; where it can, the name is set on the time as well, so a
  # record prints the clock the machine that wrote it was keeping.
  zone <- Sys.timezone()
  skip_if(is.na(zone) || !nzchar(zone), "the zone cannot be named here")
  expect_equal(attr(rec$time, "tzone"), zone)
  expect_equal(rec$timezone, zone)
  expect_equal(format(rec$time, tz=rec$timezone), format(rec$time))
})

test_that("a zone that cannot be named is recorded as the clock's abbreviation", {
  testthat::local_mocked_bindings(
    Sys.timezone=function(...) NA_character_, .package="base"
  )
  timed <- .recordTime()
  expect_equal(timed$timezone, format(timed$time, "%Z"))
  # Nothing R cannot read back is set on the time itself.
  expect_null(attr(timed$time, "tzone"))
})

test_that("a record says which versions of which packages ran", {
  tw <- tagWave(.recWave())
  own <- getNamespaceVersion(asNamespace("sonicscrewdriver"))[[1]]

  # The same call can give a different answer under another version of the
  # package it comes from, or of this one, which decides what the function is
  # given and how the record is made.
  rec <- tagApply(tw, tuneR::normalize, unit="1")@processing[[1]]
  expect_equal(
    rec$versions,
    c(tuneR=getNamespaceVersion(asNamespace("tuneR"))[[1]], sonicscrewdriver=own)
  )
  # Next to the function it describes, ahead of the digests added once the
  # function has run.
  expect_equal(which(names(rec) == "versions"), which(names(rec) == "func") + 1)

  # A function of this package is this package's alone.
  expect_equal(normalise(tw)@processing[[1]]$versions, c(sonicscrewdriver=own))

  # A function of base R, closure or primitive, is R's.
  r_version <- as.character(getRversion())
  expect_equal(
    tagApply(tw, base::identity)@processing[[1]]$versions,
    c(base=r_version, sonicscrewdriver=own)
  )
  expect_equal(
    tagApply(tw, `(`)@processing[[1]]$versions,
    c(base=r_version, sonicscrewdriver=own)
  )

  # A function written outside any package has no package of its own to name.
  user_fun <- function(x) x
  environment(user_fun) <- globalenv()
  expect_equal(tagApply(tw, user_fun)@processing[[1]]$versions, c(sonicscrewdriver=own))

  # The record a result that is not a wave carries says the same.
  sp <- tagApply(tagWave(noise("white", duration=4410, samp.rate=44100)),
                 seewave::spec, plot=FALSE)
  expect_equal(
    attr(sp, "processing")[[1]]$versions,
    c(seewave=getNamespaceVersion(asNamespace("seewave"))[[1]], sonicscrewdriver=own)
  )

  # A record made by hand keeps the two fields every record once had.
  expect_named(addProcess(tw, "by hand")@processing[[1]], c("process", "output"))
})

test_that("a function is placed in the package it was written in", {
  expect_equal(.funPackage(seewave::ffilter), "seewave")
  expect_equal(.funPackage(cutws), "sonicscrewdriver")
  expect_null(.funPackage(sum))
  user_fun <- function(x) x
  environment(user_fun) <- globalenv()
  expect_null(.funPackage(user_fun))
})

test_that("a function that was stored rather than called is named from itself", {
  # Named as it would be called from outside the package it was written in.
  expect_equal(.functionName(seewave::bwfilter), "seewave::bwfilter")
  expect_equal(.functionName(.ffilterWave), "sonicscrewdriver:::.ffilterWave")
  expect_equal(.functionName(`(`), "base::(")

  # Written at the top level, it goes by the name it was given there.
  stored <- function(x) x
  environment(stored) <- globalenv()
  assign(".ssdStoredFilter", stored, envir=globalenv())
  withr::defer(rm(".ssdStoredFilter", envir=globalenv()))
  expect_equal(.functionName(stored), ".ssdStoredFilter")

  # Written inline, it has no name to go by.
  expect_equal(.functionName(function(x) x), "<anonymous function>")
  expect_equal(.functionName(new("WaveFilter")@func), "<anonymous function>")

  # Two names for code that is the same cannot be told apart, and either says
  # what ran: an exported name is preferred, then the first alphabetically.
  expect_equal(.functionName(base::identity), "base::dontCheck")
})

test_that("a record says which digest its hashes are", {
  tw <- tagWave(.recWave())
  # The sketch is the default: a full digest of an hour-long recording would be
  # minutes of hashing per step, which is enough to make a record not worth
  # keeping.
  expect_equal(
    tagApply(tw, tuneR::normalize, unit="1")@processing[[1]]$hash_method,
    "sketch"
  )

  withr::local_options(sonicscrewdriver.provenance="full")
  expect_equal(
    tagApply(tw, tuneR::normalize, unit="1")@processing[[1]]$hash_method,
    "full"
  )
})

test_that("a sketch of a short recording still reads every sample", {
  # What makes the sketch defensible as the default: the blocks cover anything
  # short whole, so for a recording of up to some seconds it is as strong as
  # the full digest and a change to any sample changes it.
  w <- .recWave(4410)
  sketch <- .waveHash(w, "sketch")
  expect_equal(length(.waveSketch(w@left)), length(w@left))
  for (i in c(1, 2205, 4410)) {
    edited <- w
    edited@left[i] <- edited@left[i] + 1
    expect_false(identical(.waveHash(edited, "sketch"), sketch))
  }
})

test_that("the sketch digest costs the same whatever the recording's length", {
  # Long enough that the blocks do not cover it.
  w <- .recWave(50000)
  full <- .waveHash(w, "full")
  sketch <- .waveHash(w, "sketch")
  expect_match(sketch, "^[0-9a-f]{64}$")
  expect_false(identical(full, sketch))

  # It tells an edited recording from an unedited one.
  edited <- w
  edited@left[1] <- edited@left[1] + 1
  expect_false(identical(.waveHash(edited, "sketch"), sketch))
  # Including one whose length alone has changed.
  shorter <- w
  shorter@left <- shorter@left[-1]
  expect_false(identical(.waveHash(shorter, "sketch"), sketch))

  # But it is not evidence that two recordings are the same: a sample between
  # the blocks is not read, which is the whole of what it buys and why a record
  # says which digest it holds.
  between <- w
  between@left[4500] <- between@left[4500] + 1
  expect_equal(.waveHash(between, "sketch"), sketch)
  expect_false(identical(.waveHash(between, "full"), full))

  # The sketch is what a record holds unless it is asked otherwise.
  rec <- tagApply(tagWave(w), tuneR::normalize, unit="1")@processing[[1]]
  expect_equal(rec$hash_method, "sketch")
  expect_equal(rec$input_hash, sketch)

  # And the full digest is there for a history that has to be evidence.
  withr::local_options(sonicscrewdriver.provenance="full")
  rec <- tagApply(tagWave(w), tuneR::normalize, unit="1")@processing[[1]]
  expect_equal(rec$hash_method, "full")
  expect_equal(rec$input_hash, full)
})

test_that("a sketch of a vector the blocks would cover is all of it", {
  expect_equal(.waveSketch(1:10), 1:10)
  long <- seq_len(50000)
  sketched <- .waveSketch(long)
  expect_equal(length(sketched), 4096 * 10)
  # The blocks reach both ends, so a recording trimmed at either is covered.
  expect_equal(sketched[1], 1)
  expect_equal(sketched[length(sketched)], 50000)
})

test_that("an unknown provenance option warns and the default is used", {
  withr::local_options(sonicscrewdriver.provenance="cheap")
  expect_warning(method <- .hashMethod(), "must be 'sketch' or 'full'")
  expect_equal(method, "sketch")

  withr::local_options(sonicscrewdriver.provenance=c("full", "sketch"))
  expect_warning(expect_equal(.hashMethod(), "sketch"))
})

test_that("a chain of operations reads as one history", {
  tw <- tagWave(noise("white", duration=44100, samp.rate=44100), origin="demo")
  tw@metadata <- list(site="here")
  tw <- cutws(tw, from=0, to=22050)
  tw <- normalise(tw)
  tw <- tagApply(
    tw, seewave::ffilter, from=1000, to=4000, output="Wave",
    label="band-pass 1-4 kHz"
  )

  expect_equal(length(tw@processing), 3)
  expect_equal(
    vapply(tw@processing, function(r) r$func, character(1)),
    c("cutws", "normalise", "seewave::ffilter")
  )
  expect_equal(tw@processing[[3]]$process, "band-pass 1-4 kHz")
  # The tags the wave started with survive every step.
  expect_equal(tw@origin, "demo")
  expect_equal(tw@metadata$site, "here")

  # Each step says what it was applied to, so the order is not taken on trust:
  # a step's output is the next step's input, and the last is the wave in hand.
  for (i in seq_len(2)) {
    expect_equal(tw@processing[[i]]$output_hash, tw@processing[[i + 1]]$input_hash)
  }
  expect_equal(tw@processing[[3]]$output_hash, .waveHash(tw))
  # Every step says what it was given, and when.
  for (rec in tw@processing) {
    expect_true(length(rec$params) > 0)
    expect_true(nzchar(rec$timezone))
    expect_equal(rec$hash_method, "sketch")
  }
})

test_that("the same operation twice is recorded twice", {
  tw <- tagWave(.recWave())
  once <- normalise(tw, unit="1")
  twice <- normalise(once, unit="1")

  expect_equal(length(once@processing), 1)
  expect_equal(length(twice@processing), 2)
  # The earlier record is untouched by the later one.
  expect_equal(twice@processing[[1]], once@processing[[1]])
  # And the second says it was applied to what the first gave back.
  expect_equal(twice@processing[[2]]$input_hash, once@processing[[1]]$output_hash)
  expect_equal(twice@processing[[2]]$params, list(unit="1"))
})

test_that("an untagged wave is still returned untouched and unrecorded", {
  w <- .recWave()
  cut <- cutws(w, from=0, to=2205)
  expect_false(.isTagged(cut))
  expect_equal(cut, cutws(w, from=0, to=2205))

  # A whole chain of them, as the new fields are gathered on the recording path
  # alone and must not reach a wave that has nowhere to keep them.
  done <- tagApply(normalise(cut), tuneR::normalize, unit="1")
  expect_false(.isTagged(done))
  expect_null(attr(done, "processing"))
})
