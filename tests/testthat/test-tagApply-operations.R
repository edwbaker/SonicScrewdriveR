# Every operation of this package that changes a wave records itself on a tagged
# one, through tagApply(), and gives exactly what it always gave on an untagged
# one. A history with gaps in it looks like nothing having happened.

.opsWave <- function() {
  set.seed(1)
  tuneR::Wave(round(runif(4410, -1000, 1000)), samp.rate=44100, bit=16)
}

.opsWaveMC <- function() {
  w <- .opsWave()
  mc <- tuneR::WaveMC(cbind(w@left, rev(w@left)), samp.rate=44100, bit=16)
  colnames(mc@.Data) <- c("A", "B")
  return(mc)
}

# Compared slot by slot rather than whole: tuneR::normalize() leaves a
# `scaled:center` attribute on a WaveMC, which is debris from scale() rather
# than part of the recording, and an S4 coercion does not carry it.
.expectSameWave <- function(a, b) {
  expect_equal(class(a)[1], class(b)[1])
  for (s in methods::slotNames(b)) {
    expect_equal(methods::slot(a, s), methods::slot(b, s), info=s)
  }
}

# The same operation on the same wave, tagged and untagged: the audio must not
# depend on the tags, and only the tagged one is recorded. Both runs are given
# the same random stream, as some of these operations draw noise, which means
# the wave has to be built before the first of the two seeds is set.
.expectRecorded <- function(op, func, w, ...) {
  force(w)
  set.seed(7)
  plain <- op(w, ...)
  expect_false(.isTagged(plain))

  tagged_in <- tagWave(w, origin="test")
  tagged_in@metadata <- list(site="here")
  set.seed(7)
  tagged <- op(tagged_in, ...)

  expect_true(.isTagged(tagged))
  .expectSameWave(untagWave(tagged), plain)
  expect_equal(tagged@origin, "test")
  expect_equal(tagged@metadata$site, "here")

  expect_equal(length(tagged@processing), 1)
  rec <- tagged@processing[[1]]
  expect_equal(rec$func, func)
  expect_equal(rec$input_hash, .waveHash(tagged_in))
  expect_equal(rec$output_hash, .waveHash(tagged))
  expect_s3_class(rec$time, "POSIXct")
  expect_gte(rec$duration, 0)
  return(rec)
}

test_that("normalise records what it did", {
  rec <- .expectRecorded(normalise, "normalise", .opsWave(), unit="1")
  expect_equal(rec$params$unit, "1")

  rec <- .expectRecorded(normalise, "normalise", .opsWaveMC(), unit="1")
  expect_equal(rec$params$unit, "1")

  # The unit that was used is recorded, not the NULL that was asked for.
  rec <- .expectRecorded(normalise, "normalise", .opsWave())
  expect_equal(rec$params$unit, "16")

  # Nothing was checked here before, so anything without a bit slot failed on
  # the slot rather than on being the wrong sort of thing.
  expect_error(normalise(1:10), "Expecting a Wave or WaveMC object")
  expect_error(normalise("sheep", unit="1"), "Expecting a Wave or WaveMC object")
})

test_that("upsample records what it did", {
  rec <- .expectRecorded(upsample, "upsample", .opsWave(), upsample.rate=88200)
  expect_equal(rec$params$upsample.rate, 88200)
  expect_equal(rec$params$method, "basic")

  stereo <- tuneR::stereo(.opsWave(), .opsWave())
  .expectRecorded(upsample, "upsample", stereo, upsample.rate=88200)

  # A function given as the method is named rather than written into the record.
  rec <- .expectRecorded(
    upsample, "upsample", .opsWave(),
    upsample.rate=88200, method=function(x) ifelse(is.na(x), 0, x)
  )
  expect_equal(rec$params$method, "<function>")
})

test_that("cutws records the samples it cut to", {
  rec <- .expectRecorded(cutws, "cutws", .opsWave(), from=10, to=200)
  expect_equal(rec$params$from, 10)
  expect_equal(rec$params$to, 200)
  expect_false(rec$input_hash == rec$output_hash)

  .expectRecorded(cutws, "cutws", .opsWaveMC(), from=10, to=200)
  .expectRecorded(cutws, "cutws", tuneR::stereo(.opsWave(), .opsWave()), from=10, to=200)

  # The end asked for is recorded as the end that was cut to, as Inf and a
  # sample beyond the wave both come back as the final sample.
  rec <- .expectRecorded(cutws, "cutws", .opsWave(), from=4401, to=Inf)
  expect_equal(rec$params$to, 4410)

  # seewave draws the plot and rejects a tagged wave, so an untagged one is cut.
  expect_silent(cut <- cutws(tagWave(.opsWave()), from=10, to=200, plot=TRUE))
  expect_true(.isTagged(cut))
})

test_that("generateTimeMask records what it did", {
  rec <- .expectRecorded(
    generateTimeMask, "generateTimeMask", .opsWave(),
    method="squarewave", dutyCycle=0.5
  )
  expect_equal(rec$params$dutyCycle, 0.5)
  expect_equal(rec$params$n.periods, 10)

  .expectRecorded(
    generateTimeMask, "generateTimeMask", .opsWaveMC(),
    method="squarewave", dutyCycle=0.5
  )
  .expectRecorded(
    generateTimeMask, "generateTimeMask", tuneR::stereo(.opsWave(), .opsWave()),
    method="squarewave", dutyCycle=0.5
  )
  rec <- .expectRecorded(
    generateTimeMask, "generateTimeMask", .opsWave(),
    method="random", dutyCycle=0.5
  )
  expect_equal(rec$params$method, "random")

  # Each wave of a list is masked and recorded on its own.
  res <- generateTimeMask(tagWave(list(.opsWave(), .opsWaveMC())), dutyCycle=0.5)
  expect_equal(vapply(res, function(x) length(x@processing), numeric(1)), c(1, 1))
  expect_equal(res[[2]]@processing[[1]]$func, "generateTimeMask")

  # Anything that is not Wave-like matched neither class the mask is applied to,
  # so it came back unmasked as though nothing had been asked for.
  expect_error(generateTimeMask(1:10), "wave must be a Wave-like object")
  expect_error(generateTimeMask("sheep"), "wave must be a Wave-like object")
  expect_error(
    generateTimeMask(list(.opsWave(), "sheep")),
    "wave must be a Wave-like object"
  )
})

test_that("generateNoise records its arguments rather than its name alone", {
  rec <- .expectRecorded(
    generateNoise, "generateNoise", .opsWave(),
    noise="white", noise.add=FALSE, noise.ratio=0.5, noise.ref="file"
  )
  expect_equal(rec$params$noise, "white")
  expect_equal(rec$params$noise.ratio, 0.5)
  expect_equal(rec$params$noise.ref, "file")

  .expectRecorded(
    generateNoise, "generateNoise", .opsWaveMC(),
    noise="white", noise.ratio=0.5
  )

  # Two ratios of the same noise are told apart by their records.
  tw <- tagWave(.opsWave())
  quiet <- generateNoise(tw, "white", noise.ratio=0.1)@processing[[1]]
  loud <- generateNoise(tw, "white", noise.ratio=0.9)@processing[[1]]
  expect_equal(quiet$params$noise.ratio, 0.1)
  expect_equal(loud$params$noise.ratio, 0.9)
  expect_false(quiet$output_hash == loud$output_hash)

  # Each wave of a list is recorded once, on itself.
  res <- generateNoise(tagWave(list(.opsWave(), .opsWaveMC())), "white")
  expect_equal(vapply(res, function(x) length(x@processing), numeric(1)), c(1, 1))
  expect_equal(res[[2]]@processing[[1]]$func, "generateNoise")
})

test_that("concat records the waves it concatenated on", {
  w <- .opsWave()
  other <- tagWave(w, origin="other")
  other@metadata <- list(site="there")

  tagged <- concat(tagWave(w, origin="test"), other, method="bind")
  rec <- tagged@processing[[1]]
  expect_equal(rec$process, "concat-bind")
  expect_equal(rec$func, "concat")
  expect_equal(rec$params$method, "bind")
  # The other wave keeps its own tags and brings a digest of its samples.
  expect_equal(rec$params[[1]]$origin, "other")
  expect_equal(rec$params[[1]]$metadata$site, "there")
  expect_equal(rec$params[[1]]$sha256, .waveHash(w))
  expect_equal(rec$input_hash, .waveHash(w))
  expect_equal(rec$output_hash, .waveHash(tagged))
  expect_gte(rec$duration, 0)
  .expectSameWave(untagWave(tagged), concat(w, w, method="bind"))

  # An untagged wave concatenated on is named by its digest alone.
  rec <- concat(tagWave(w), w, method="bind")@processing[[1]]
  expect_match(rec$params[[1]], "^<wave sha256:[0-9a-f]{64}>$")

  # Mixed classes concatenate nothing. A tagged wave was returned unchanged,
  # with a record of a concatenation that had not happened.
  expect_error(
    concat(tagWave(w), tuneR::WaveMC(w), method="bind"),
    "All objects must be Wave-like objects of the same type."
  )
})

test_that("generateTimeShift records each shifted wave once", {
  for (make in list(.opsWave, .opsWaveMC)) {
    w <- make()
    tw <- tagWave(w, origin="test")

    for (type in c("silent", "rotate")) {
      plain <- generateTimeShift(w, type=type, amount=c(0.01, 0.02), where="both")
      tagged <- generateTimeShift(tw, type=type, amount=c(0.01, 0.02), where="both")
      expect_equal(length(tagged), length(plain))
      for (i in seq_along(plain)) {
        expect_false(.isTagged(plain[[i]]))
        expect_true(.isTagged(tagged[[i]]))
        .expectSameWave(untagWave(tagged[[i]]), plain[[i]])
        expect_equal(length(tagged[[i]]@processing), 1)
        rec <- tagged[[i]]@processing[[1]]
        expect_equal(rec$func, "generateTimeShift")
        expect_equal(rec$input_hash, .waveHash(tw))
        expect_equal(rec$output_hash, .waveHash(tagged[[i]]))
      }
    }

    # Silence inserted at both ends gives two waves per amount, recorded under
    # the end the silence went in at. A rotation gives one.
    tagged <- generateTimeShift(tw, type="silent", amount=c(0.01, 0.02), where="both")
    expect_equal(
      vapply(tagged, function(x) x@processing[[1]]$params$where, character(1)),
      c("start", "end", "start", "end")
    )
    expect_equal(
      vapply(tagged, function(x) x@processing[[1]]$params$amount, numeric(1)),
      c(0.01, 0.01, 0.02, 0.02)
    )
    expect_length(generateTimeShift(tw, type="rotate", amount=c(0.01, 0.02), where="both"), 2)

    # A rotation by a whole wave leaves the audio where it was, and is still a
    # step that was taken.
    whole <- generateTimeShift(tw, type="rotate", amount=length(w)/w@samp.rate)[[1]]
    expect_equal(length(whole@processing), 1)
    expect_equal(whole@processing[[1]]$input_hash, whole@processing[[1]]$output_hash)
  }

  # Each wave of a list is shifted and recorded on its own.
  res <- generateTimeShift(tagWave(list(.opsWave(), .opsWaveMC())), amount=0.01)
  expect_equal(length(res[[1]][[1]]@processing), 1)
  expect_equal(res[[2]][[1]]@processing[[1]]$func, "generateTimeShift")
})

test_that("autoBandPass records what it did", {
  skip_if_not_installed("seewave")
  set.seed(42)
  w <- tuneR::Wave(round(10000*rnorm(8000)), samp.rate=8000, bit=16)
  w <- seewave::ffilter(w, from=1800, to=2200, output="Wave")

  rec <- .expectRecorded(autoBandPass, "autoBandPass", w, bw="-3dB", n.bw=1, lowcut=1000)
  expect_equal(rec$params$bw, "-3dB")
  expect_equal(rec$params$n.bw, 1)
  expect_equal(rec$params$lowcut, 1000)

  # Two band widths of the same filter are told apart by their records.
  tw <- tagWave(w)
  narrow <- autoBandPass(tw, n.bw=1)@processing[[1]]
  wide <- autoBandPass(tw, n.bw=3)@processing[[1]]
  expect_equal(narrow$params$n.bw, 1)
  expect_equal(wide$params$n.bw, 3)
  expect_false(narrow$output_hash == wide$output_hash)
})

test_that("filterWave records the settings of the filter", {
  skip_if_not_installed("seewave")
  w <- tuneR::noise("white", duration=4410, samp.rate=44100)
  tw <- tagWave(w, origin="test")
  tw@metadata <- list(site="here")

  low <- filterWave(tw, bandpass(from=1000, to=2000))
  rec <- low@processing[[1]]
  expect_equal(rec$func, "filterWave")
  expect_equal(rec$params$from, 1000)
  expect_equal(rec$params$to, 2000)
  expect_equal(rec$input_hash, .waveHash(tw))
  expect_equal(rec$output_hash, .waveHash(low))
  expect_gte(rec$duration, 0)
  expect_s3_class(rec$time, "POSIXct")
  expect_equal(low@origin, "test")
  expect_equal(low@metadata$site, "here")

  # The same filter at two cut-offs is no longer indistinguishable.
  high <- filterWave(tw, bandpass(from=5000, to=6000))
  expect_equal(high@processing[[1]]$params$from, 5000)
  expect_false(rec$output_hash == high@processing[[1]]$output_hash)

  # A filter with no description is recorded under the name of the function, as
  # any other call is, rather than under NA.
  expect_equal(rec$process, "filterWave")
  described <- new("WaveFilter", func=seewave::ffilter, description="band",
                   params=list(from=1000, to=2000, output="Wave"))
  expect_equal(filterWave(tw, described)@processing[[1]]$process, "band")
  expect_null(.filterLabel(new("WaveFilter", description=character(0))))

  # Every channel of a multi-channel wave is filtered, and recorded once.
  mc <- tagWave(tuneR::WaveMC(cbind(w@left, rev(w@left)), samp.rate=44100, bit=16))
  filtered <- filterWave(mc, bandpass(from=1000, to=2000))
  expect_s4_class(filtered, "TaggedWaveMC")
  expect_equal(length(filtered@processing), 1)
  expect_equal(filtered@processing[[1]]$params$to, 2000)
  .expectSameWave(
    untagWave(filtered),
    filterWave(untagWave(mc), bandpass(from=1000, to=2000))
  )
})

test_that("an operation inside tagApply is recorded once, by the outer call", {
  tw <- tagWave(.opsWave())

  res <- tagApply(tw, cutws, from=5, to=50)
  expect_equal(length(res@processing), 1)
  expect_equal(res@processing[[1]]$func, "cutws")
  expect_equal(res@processing[[1]]$params$to, 50)

  # generateTimeShift uses cutws() and concat(), which record themselves. Its
  # own record is the only one made.
  res <- generateTimeShift(tw, amount=0.01)[[1]]
  expect_equal(length(res@processing), 1)
  expect_equal(res@processing[[1]]$func, "generateTimeShift")

  # And one operation after another appends rather than replacing.
  res <- normalise(cutws(tw, from=5, to=50), unit="1")
  expect_equal(length(res@processing), 2)
  expect_equal(
    vapply(res@processing, function(x) x$func, character(1)),
    c("cutws", "normalise")
  )
  # The digests chain: what came out of the one went into the next.
  expect_equal(res@processing[[1]]$output_hash, res@processing[[2]]$input_hash)
})
