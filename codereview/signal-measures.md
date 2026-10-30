# Code review: signal measures and wave manipulation

Area reviewed: `R/frequency.R`, `R/frequencyStats.R`, `R/specStats.R`,
`R/spectraMaths.R`, `R/entropyStats.R`, `R/zerocrossing.R`, `R/ste.R`, `R/ntd.R`,
`R/normalise.R`, `R/windowing.R`, `R/upsample.R`, `R/corWaveMC.R`, `R/disp.R`,
`R/fcis.R`, `R/sheepFrequencyStats.R`, `R/jitter.R`, `R/shimmer.R`,
`R/generateNoise.R`, `R/generateTimeMask.R`, `R/generateTimeShift.R`,
`R/sweptsine.R`, `R/autoBandPass.R`, `R/cutws.R`, `R/allChannels.R`,
`R/WaveFilter.R`, `R/Wave-methods.R`, `R/PseudoWave.R`.

Correctness only. 19 findings, most severe first.

## How these were verified, and what to trust

R 4.3.3 was installed from Debian. CRAN is blocked by this container's network
policy, and installing the CRAN sources of `tuneR` and `seewave` (cloned from
the `cran` GitHub mirror) was refused by the sandbox, so the package could not
be built against its real dependencies. Debian in this image has no
`r-cran-tuner`, `r-cran-seewave` or `r-cran-signal`.

Instead, `tuneR` and `seewave` were replaced by stand-ins written for this
review, and `sonicscrewdriver` was installed and exercised against them. The
`tuneR` stand-in is **transcribed from the real tuneR 1.4.7 sources**, which were
available on disk for reading: the `Wave`/`WaveMC` classes and their `setValidity`
rules, `normalize`, `bind`, `equalWave`, `channel`, `stereo`, `prepComb`, and the
`preWaveform`/`postWaveform` generator pipeline are copies of the real code, so
`Wave` object semantics (including the `bit`/`pcm` validity rules and the fact
that `sine()`/`noise()`/`silence()` return `bit=32, pcm=FALSE`) are faithful. The
`seewave` stand-in is **not** faithful: `meanspec`, `spectro`, `ffilter` and
`corenv` are rough re-implementations.

Each finding below therefore says what its evidence is:

* **CONFIRMED** — reproduced by running the package's own code, where the result
  depends only on that code, on base R, and on `Wave` semantics transcribed from
  the real tuneR. A stand-in cannot create or hide these.
* **CONFIRMED (dependency source)** — the defect is in how this package calls
  `seewave`, and the evidence is the real `seewave` 2.2.3 source read from disk
  (quoted in the finding), not the stand-in.

Nothing here rests on a number produced by the unfaithful `seewave` stand-in.
Every numeric claim about a spectrum was made by passing `frequencyStats()` a
hand-built spectrum through its own `wave_spec` argument, or by calling
`.fcisIndexFunctions` on a hand-built amplitude matrix, so `meanspec`/`spectro`
are out of the path.

The existing tests for this area were run. They report **no genuine failures**:
every failure and error observed was traceable to a gap in the stand-ins
(`readWave` of the bundled WAV, `makeForkCluster`, the crude `corenv`) or to
internal functions not being visible in the ad-hoc harness. The bugs below all
sit in paths the suite does not cover; where a specific coverage gap hides a
finding, the finding says so.

---

## 1. `R/PseudoWave.R:121` — a `PseudoWave`'s offset is never scaled, so lazy arithmetic disagrees with eager arithmetic

`depseduoWave()` always applies `scale` first and `offset` second
(`w@left <- (w@left * pw@scale) + pw@offset`), but the operator methods just
accumulate into the two slots independently (`*` at :175, `/` at :189, `+` at
:203, `-` at :217). The order the caller wrote the operators in is discarded, so
any offset applied *before* a scale is applied *after* it instead.

Failure scenario (CONFIRMED). With `g` the generator's samples:

```r
w  <- tuneR::Wave(rep(0, 8), samp.rate = 8000, bit = 32, pcm = FALSE)
pw <- pseudoWave("sine", params = list(f0 = 1))

(w + (pw + 5) * 2)@left   # gives g*2 + 5  -> 5 at sample 1
(w + pw)@left * 2 + 10    # the eager result is (g + 5)*2 = g*2 + 10 -> 10
```

Measured: `(pw + 5) * 2` → 5, want 10. `2 * (pw + 5)` → 5, want 10.
`(pw - 1) * 3` → -1, want -3. `(pw * 2) + 5` → 5, correct (scale happens to come
first). The class documents itself as "converted to a Wave object when operated
on", so a user has no way to see that the operations were reordered; a
`PseudoWave` built as `(pseudoWave("noise", "white") + 1) * 0.5`, intending
unipolar noise at half scale, instead gets bipolar noise at half scale with a
full-size DC offset.

Test coverage: `tests/testthat/test-PseudoWave.R` exercises `scale` and `offset`
in isolation (lines 55-93) and never combines a scale with an offset, which is
exactly what hides this.

## 2. `R/ntd.R:25` — `ntd()` drops the `pcm` slot, mislabelling float waves and failing outright on 64-bit waves

Each interval is rebuilt with `tuneR::Wave(..., samp.rate = wave@samp.rate,
bit = wave@bit)` and no `pcm` argument, so `pcm` falls back to `tuneR`'s default
of `TRUE` whatever the input was.

Failure scenario (CONFIRMED):

```r
w <- tuneR::Wave(sin(2*pi*(0:39)/10), samp.rate = 8000, bit = 64, pcm = FALSE)
ntd(w, events = c(1, 11, 21, 31), FUN = function(wave) wave@pcm)
#> Error: invalid class "Wave" object: pcm Wave objects must have a resolution < 64 bit
```

tuneR's own validity rule (`R/Wave.R`, `setValidity("WaveGeneral")`) forbids
`pcm && bit == 64`, so `ntd()` cannot be used at all on a 64-bit float
recording. With `bit = 32, pcm = FALSE` — which is what every `tuneR` generator,
`sweptsine()` and `data2Wave()` produce — there is no error but each interval
wave reaches `FUN` declaring itself 32-bit integer PCM while holding samples in
[-1, 1]: measured `pcm = TRUE` for an input with `pcm = FALSE`. Any `FUN` that
reads the format (`normalise()`, `.waveFullScale()`, `writeWave()`) then works
from the wrong full scale by a factor of 2^31.

## 3. `R/autoBandPass.R:69,98` — a stereo wave comes back mono, with the right channel silently discarded

`.autoBandPass()` calls `seewave::ffilter()` directly, twice, with no channel
handling. `validateIsWave()` at :32 accepts a stereo `Wave`.

Failure scenario (CONFIRMED; the mono-isation itself is CONFIRMED (dependency
source)). In real seewave 2.2.3, `ffilter` routes its input through `inputw()`,
whose default is `channel = 1`:

```r
if(class(wave)[1]=="Wave") { ...
  if(channel==1) {wave <- as.matrix(wave@left)} ... }
```

and returns it through `outputw()`, which for `format == "Wave"` builds
`Wave(left=wave, samp.rate=f, bit=bit)` — a mono wave. So:

```r
st <- tuneR::Wave(L, R, samp.rate = 8000, bit = 32, pcm = FALSE)  # two channels
autoBandPass(st, lowcut = 100)@stereo
#> FALSE
```

Measured: stereo in, `@stereo == FALSE` out, 4096 samples, right channel gone.
`filterWave()` solves this problem — `.filterAllChannels()` in
`R/WaveFilter.R:142` exists precisely because "the filters read the left slot of
a Wave, so passing a stereo Wave or a WaveMC straight to one returned only its
first channel, silently discarding the rest" — and `autoBandPass()` has not been
given the same treatment, even though it is a filter too. Verified by comparison
in the same run: `filterWave(st, bandpass(from=100, to=1000))@stereo` is `TRUE`.

Secondary, PLAUSIBLE (the stand-in differs here so it was not reproduced): the
real `outputw()` does not pass `pcm`, so the returned wave has `pcm = TRUE`
regardless. `.filterAllChannels()` repairs this by rebuilding with
`bit = w@bit, pcm = w@pcm` (`R/WaveFilter.R:152-164`); `.autoBandPass()` returns
seewave's wave untouched, so a float input is handed back declaring 32-bit
integer PCM.

## 4. `R/fcis.R:209` — a false-colour index spectrogram of a stereo recording describes only the left channel

`.fcisWindow()` calls `spectro(wave, f = wave@samp.rate, wl = wl, ...)` with no
channel argument, and `fcis()` (:294) has no `.useAllChannels()` branch, unlike
`frequencyStats()` (:51) and `entropyStats()` (:18).

Failure scenario (CONFIRMED, with the channel selection CONFIRMED (dependency
source) from the `inputw()` code quoted in finding 3 — real `spectro` calls
`inputw(wave = wave, f = f, channel = channel)` with `channel = 1` by default):

```r
L  <- sin(2*pi* 500*(0:4095)/8000)   # left:  500 Hz
R  <- sin(2*pi*3000*(0:4095)/8000)   # right: 3000 Hz
st <- tuneR::Wave(L, R, samp.rate = 8000, bit = 32, pcm = FALSE)
mo <- tuneR::Wave(L,    samp.rate = 8000, bit = 32, pcm = FALSE)

identical(fcis(st, window.length = 1024, wl = 256)$indices,
          fcis(mo, window.length = 1024, wl = 256)$indices)
#> TRUE
```

Measured: byte-identical indices and a 128 x 4 colour matrix in both cases, even
though the two channels carry different tones. The image is presented as a
summary of the recording with no indication that half of it was dropped. A
`WaveMC` is at least rejected (`windowing()`'s `validateIsWave()` raises
"Expecting a Wave object"); a stereo `Wave` is not.

## 5. `R/generateNoise.R:101` — `noise.ratio = 1` returns a wave of `NaN`

`noise.frac = 1 / (1/noise.ratio - 1)` is `1/0 = Inf` at `noise.ratio = 1`, and
line 104 then evaluates `(x + Inf * n) / (1 + Inf)`, which is `Inf/Inf`.

Failure scenario (CONFIRMED):

```r
w <- tuneR::Wave(round(sin(2*pi*(0:999)/50)*10000), samp.rate = 8000, bit = 16)
generateNoise(w, noise.ratio = 1)@left[1:3]
#> NaN NaN NaN
```

`noise.ratio` is documented without a stated range, and 1 is its natural
endpoint — "all noise, no signal". Every sample comes back `NaN`, with no
warning, and the `NaN`s then propagate into whatever the wave was generated for.
`noise.ratio = 0` is handled correctly (`noise.frac = 0`).

## 6. `R/generateNoise.R:57-70` — `noise.add` is accepted and documented but never read

`generateNoise()` passes `noise.add` through to `.generateNoise()` (:41), whose
body never mentions it. Several noise kinds are always summed into one wave
(:66-69).

Failure scenario (CONFIRMED):

```r
a <- generateNoise(w, noise = c("white", "pink"), noise.add = TRUE)
b <- generateNoise(w, noise = c("white", "pink"), noise.add = FALSE)
```

Measured: both return a single `Wave` of 1000 samples. The documentation at
`R/generateNoise.R:13-14` says "If TRUE all noise sources are added to wave. If
FALSE separate outputs are created for each noise source", and `FALSE` is the
**default**, so the documented default behaviour is not implemented: a caller
asking for one output per noise source silently gets the sum instead, and
cannot tell the two apart. (`output` is also unused, but that one is at least
flagged with a `TODO` at :17.)

## 7. `R/entropyStats.R:23` — `entropyStats()` draws a plot on every call

`spec <- meanspec(wave)` omits `plot = FALSE`. In real seewave 2.2.3 the
signature of `meanspec` is

```r
meanspec <-function(wave, f, channel = 1, wl = 512, wn = "hanning", ovlp = 0,
                    fftw = FALSE, norm = TRUE, PSD = FALSE, PMF = FALSE,
                    FUN = mean, correction = "none", dB = NULL, dBref = NULL,
                    from = NULL, to = NULL, identify = FALSE, col = "black",
                    cex = 1, plot = 1, ...)
```

— `plot = 1`, i.e. plotting is on by default.

Failure scenario — CONFIRMED (dependency source): `entropyStats(wave)` opens a
graphics device and draws a spectrum as a side effect of asking for four
numbers. Under `Rscript` that silently creates `Rplots.pdf` in the working
directory; inside `windowing(wave, window.length = 44100, FUN = entropyStats)`
over a ten-minute recording it draws roughly 600 plots. The sibling function
`frequencyStats()` passes `plot=FALSE` on the same call (`R/frequencyStats.R:65`),
so the two disagree. Not reproduced numerically because the `seewave` stand-in's
`meanspec` defaults to `plot = FALSE`; the evidence is the real signature above.

Test coverage: `tests/testthat/test-entropyStats.R` has four assertions and none
of them is an `expect_silent()` or a device check, so the side effect is
invisible to the suite.

## 8. `R/generateTimeShift.R:59` — a shift smaller than half a sample errors for a `Wave` but not for a `WaveMC`

`.silence(wave, round(amount * wave@samp.rate))` passes a sample count of `0`
to `tuneR::silence()` for any `amount < 0.5/samp.rate`, including `amount = 0`.
Real tuneR's `preWaveform()` (`R/Waveforms.R`) begins

```r
if (!is.numeric(duration) || duration <= 0 || length(duration) != 1)
    stop("'duration' must be a positive numeric of length 1")
```

so a zero-sample `silence()` is an error. The `WaveMC` branch of `.silence()`
(`:84-91`) builds its matrix itself and accepts `nrow = 0`.

Failure scenario (CONFIRMED against that transcribed check):

```r
w <- tuneR::Wave(as.numeric(1:10), samp.rate = 8000, bit = 32, pcm = FALSE)
generateTimeShift(w, type = "silent", amount = 1e-6)
#> Error: 'duration' must be a positive numeric of length 1

mc <- tuneR::WaveMC(cbind(1:10, 11:20), samp.rate = 8000, bit = 32, pcm = FALSE)
length(generateTimeShift(mc, type = "silent", amount = 0)[[1]])
#> 10
```

`amount` is a vector of shifts in seconds, so `amount = c(0, 1, 2)` — asking for
the unshifted wave alongside two shifts — aborts the whole call for a `Wave` and
succeeds for a `WaveMC`. The error names tuneR's `duration`, which the caller
never supplied. `type = "rotate"` handles the same case correctly (`:72-74`).

## 9. `R/zerocrossing.R:20`, `R/jitter.R:33`, `R/shimmer.R:22`, `R/ste.R:33` — these measures read only `@left`, so a stereo wave silently loses a channel

All four call `validateIsWave()`, which accepts a stereo `Wave`, and then index
`wave@left` directly. None has a `.useAllChannels()` branch.

Failure scenario (CONFIRMED):

```r
L <- sin(2*pi* 500*(0:4095)/8000); R <- sin(2*pi*3000*(0:4095)/8000)
st <- tuneR::Wave(L, R, samp.rate = 8000, bit = 32, pcm = FALSE)
mo <- tuneR::Wave(L,    samp.rate = 8000, bit = 32, pcm = FALSE)
```

Measured: `zerocross`, `jitter`, `shimmer` and `ste` each return results for
`st` that are identical to those for `mo`, although the right channel is a 3 kHz
tone where the left is a 500 Hz one — an honest two-channel answer could not
match. The package's own convention is the opposite: `frequencyStats()`,
`entropyStats()` and `filterWave()` all detect extra channels with
`.useAllChannels()` and return one result per channel. `.periodBoundaries()`
(`R/zerocrossing.R:54`) inherits the same limitation, which is why `jitter` and
`shimmer` do.

## 10. `R/generateNoise.R:104-111` — every channel gets the identical noise sample sequence

`n@left` is reused for the right channel (:106) and for every `WaveMC` column
(:111); a second channel of noise is never generated.

Failure scenario (CONFIRMED):

```r
ws <- tuneR::Wave(round(sin(2*pi*(0:99)/50)*10000),
                  round(cos(2*pi*(0:99)/50)*10000), samp.rate = 8000, bit = 16)
r  <- generateNoise(ws)
all.equal(r@left*2 - ws@left, r@right*2 - ws@right)
#> TRUE
```

Measured: the added noise is bit-identical in both channels, so it is perfectly
correlated across them with zero inter-channel delay. The documented purpose is
"testing of the robustness of automated identification algorithms to noise"
(:3-4); any algorithm that uses inter-channel differences — and this package
ships `tdoa()`, `bearing()` and `corWaveMC()`, which all do — sees noise that
cancels exactly between channels and is therefore not tested at all.

## 11. `R/cutws.R:30-38` — `from` has no lower bound, so `from = 0` silently returns the wrong samples

The guard checks that `from` and `to` are numeric and whole, and clamps `to` to
the end of the wave, but never checks `from >= 1`. `wave@left[0:to]` drops the
zero subscript rather than failing.

Failure scenarios (all CONFIRMED):

```r
w <- tuneR::Wave(as.numeric(1:10), samp.rate = 8000, bit = 32, pcm = FALSE)

cutws(w, from = 0,  to = 3)@left   #> 1 2 3    -- asked for 4 samples, got 3
cutws(w, from = 0,  to = 0)        #> a wave of 0 samples
cutws(w, from = -2, to = 3)        #> Error: only 0's may be mixed with negative subscripts
cutws(w, from = 3e9, to = 10)      #> Error: missing value where TRUE/FALSE needed
cutws(w, from = NA,  to = 10)      #> Error: missing value where TRUE/FALSE needed
```

`from = 0` is the dangerous one: a zero-based offset computed by a caller gives
a section one sample short, shifted by one sample, with no error — and the
documented behaviour for `to` beyond the end is to clamp, so clamping `from`
below 1 is what a caller would expect. The last two cases come from
`as.integer(from) == from` at :30: `as.integer(3e9)` is `NA` with a coercion
warning, and `NA` as the condition of `if` aborts with R's own message rather
than "In cutws both from and to must be integers".

## 12. `R/frequencyStats.R:106` — the spectral flatness ignores `lowcut`

The flatness is taken from `wave_spec`, the untouched spectrum, while every
other statistic is computed from `y`, the copy with everything below `lowcut`
zeroed (:75).

Failure scenario (CONFIRMED, with a hand-built spectrum so `meanspec` is out of
the path):

```r
spec <- cbind(x = 0:9, y = c(9, 9, 1, 2, 3, 2, 1, 0, 0, 0))   # kHz
wv   <- tuneR::Wave(rep(0.1, 100), samp.rate = 20000, bit = 32, pcm = FALSE)
for (lc in c(0, 2000, 4000))
  frequencyStats(wv, wave_spec = spec, lowcut = lc, output.unit = "Hz", warn = FALSE)
```

Measured: `peak` moves from 0 Hz to 4000 Hz to 4000 Hz as `lowcut` rises, while
`spectral flatness` stays at 0.989568 for all three. A caller raising `lowcut`
to exclude wind or handling noise gets peak, bandwidth and Q measured on the
clean band but a flatness still dominated by the rumble they asked to exclude,
and the two numbers are returned side by side in one list as though they
described the same thing.

## 13. `R/upsample.R:57` — upsampling an integer-PCM wave leaves fractional samples in it while still declaring PCM

`.upsampleChannel()` interpolates linearly and `.upsample()` rebuilds the wave
with `bit=wave@bit, pcm=wave@pcm` (:41-45), so the declared format no longer
matches the data.

Failure scenario (CONFIRMED):

```r
w <- tuneR::Wave(c(0, 1000, 0, -1000), samp.rate = 4, bit = 16, pcm = TRUE)
upsample(w, 12)@left
#> 0 333.3333 666.6667 1000 666.6667 333.3333 0 -333.3333 -666.6667 -1000 -1000 -1000
```

Measured: `bit = 16`, `pcm = TRUE`, and the samples are not integers. tuneR's
`setValidity` does not check integrality, so nothing complains until
`writeWave()` truncates them, or until a later step reasons from `bit`/`pcm`.
`normalise()` and `.filterAllChannels()` both take care to keep format and data
consistent; `upsample()` does not. Rounding the interpolated samples for a PCM
input, or returning a float wave, would both be consistent.

## 14. `R/ntd.R:25` — adjacent intervals share their boundary sample

`wave@left[events[i]:events[i+1]]` is inclusive at both ends, so an interval of
`n` samples is cut as `n+1` samples and each boundary sample is counted twice.

Failure scenario (CONFIRMED):

```r
w <- tuneR::Wave(as.numeric(1:21), samp.rate = 8000, bit = 32, pcm = TRUE)
ntd(w, events = c(1, 11, 21), FUN = function(wave) length(wave))[, 2]
#> 11 11                  -- the intervals are 10 samples long
ntd(w, events = c(1, 11, 21), FUN = function(wave) sum(wave@left))[, 2]
#> 66 176                 -- sums to 242, where the whole wave sums to 231
```

Measured: sample 11 is in both intervals, so the natural-time-domain values no
longer partition the signal. For the short intervals this function exists to
measure — a pulse train from `pulseDetection()` — a one-sample overlap on an
interval of a few tens of samples is a systematic few-percent bias in anything
energy-like. `events[i]:(events[i+1]-1)` would be the half-open convention the
rest of the package uses (`windowing()` at `R/windowing.R:80` uses
`start + window.length - 1`).

## 15. `R/windowing.R:91` — an overlap at or above the window length gives an error about `seq_len`

`n.windows <- ceiling(n.samples / (window.length - window.overlap))` divides by
zero or by a negative number, and `seq_len()` at :95 then rejects the result.

Failure scenario (CONFIRMED):

```r
w <- tuneR::Wave(as.numeric(1:1000), samp.rate = 8000, bit = 32, pcm = FALSE)
windowing(w, window.length = 1000, window.overlap = 1000, FUN = function(...) 1)
#> Error: argument must be coercible to non-negative integer
windowing(w, window.length = 1000, window.overlap = 1200, FUN = function(...) 1)
#> Error: argument must be coercible to non-negative integer
```

An overlap equal to the window length means a zero hop and so cannot be honoured,
but the message names neither argument. The function already validates
`bind.wave` against a positive overlap at :85-88, so a check here would be in
keeping. Low severity: the input is genuinely impossible, only the diagnosis is
wrong.

## 16. `R/fcis.R:13` — `scaleRGB()` truncates instead of rounding, biasing every colour channel down one level

`as.integer(vector)` truncates toward zero after the rescaling at :11.

Failure scenario (CONFIRMED):

```r
sonicscrewdriver:::scaleRGB(0:2)
#> 0 127 255          -- the midpoint scales to 127.5 and should map to 128
```

Every channel value that is not exactly on an integer loses up to one level, so
a flat ramp comes out slightly dark and, more to the point, two index values
that differ by less than one level can map to the same byte in one direction but
not the other. `round()` is what the rescaling implies.

## 17. `R/PseudoWave.R:172-248` — `numeric - PseudoWave` and `numeric / PseudoWave` have no method

Methods exist for `PseudoWave * numeric` (:172), `/` (:186), `+` (:200),
`-` (:214), and for `numeric * PseudoWave` (:228) and `numeric + PseudoWave`
(:242), but not for the two non-commutative operators in the reversed order.

Failure scenario (CONFIRMED):

```r
pw <- pseudoWave("sine", params = list(f0 = 1))
5 - pw   #> Error in 5 - pw : non-numeric argument to binary operator
5 / pw   #> Error in 5 / pw : non-numeric argument to binary operator
```

`1 - pseudoWave("noise", "white")` is the obvious way to write an inverted
noise source, and the symmetry of the methods that are present suggests it
should work. The error is R's generic one and says nothing about `PseudoWave`.

## 18. `R/ntd.R:34`, `R/spectraMaths.R:72` — normalising something that is entirely zero gives `NaN` without a word

Both divide by a quantity that is zero when their input is: `ntd()` by the total,
`Fk <- Fk/sum(Fk)`, and `normaliseSpectrum()` by the largest bin,
`m <- max(s1[,2]); s1[,2] <- s1[,2]/m`.

Failure scenario (CONFIRMED):

```r
ntd(w, events = c(1, 6, 11), FUN = function(wave) 0, normalise = TRUE)[, 2]
#> NaN NaN
sonicscrewdriver:::normaliseSpectrum(cbind(0:4, rep(0, 5)))[, 2]
#> NaN NaN NaN NaN NaN
```

`normalise = TRUE` is documented as producing "a probability density"
(`R/ntd.R:9`), and a silent recording — or any `FUN` that returns zero for every
interval — produces `NaN` instead. `normaliseSpectrum()` is internal but is
reached from `tests/testthat/test-spectraMaths.R:13`; a spectrum of digital
silence gives an all-`NaN` spectrum. Passing that on to `addSpectra()` or
`specStats()` does not raise anything either, because `validateSpectrum()`
tests amplitudes with `is.na()`, which is also true of `NaN`, and so quietly
replaces every one of them with zero — the failure is swallowed rather than
reported.

## 19. `R/sweptsine.R:35,43,66,74` — a non-integer sample count is silently rounded, and the sweep's duration is then not the one used for its phase

`vector_length <- samp.rate*sweep.time` is not required to be a whole number,
and `seq(from=0, by=..., length=vector_length)` rounds it, while
`sweep.duration <- vector_length / samp.rate` (:43) keeps the unrounded value
and is what sets the instantaneous frequency at the end of the sweep.

Failure scenario (CONFIRMED):

```r
length(sweptsine(1000, 4000, sweep.time = 0.5, time.unit = "samples",
                 output = "vector"))
#> 1                                   -- a one-sample "sweep"
length(sweptsine(1000, 4000, sweep.time = 1.00005, samp.rate = 44100,
                 output = "vector"))
#> 44103                               -- 44102.205 samples were asked for
```

A fractional `sweep.time` in samples gives a wave of one sample with no warning.
In seconds, the generated wave is a different length from the `sweep.duration`
the phase was integrated over, so the frequency at the last sample is not quite
`f1`. Both are small, but `time.unit = "samples"` with a fractional value is a
plain input error that is accepted.

---

## Checked and found correct

Recording these so the next reviewer does not redo them.

* **Frequency-bin to Hz arithmetic in `frequencyStats()`.** The `regions_l[...] + 1`
  offset at `R/frequencyStats.R:205-209` is right: a hand-built spectrum of bins
  at 0-9 kHz with amplitudes `c(0,0,1,2,3,2,1,0,0,0)` gives the -10 dB region as
  min 2000 Hz, max 6000 Hz, centre 4000 Hz, bandwidth 4000 Hz, Q 1 — exactly the
  expected values. `in.peak <- max(which(regions_l < peak.index))` always selects
  the region containing the peak, including when the peak is in the first or last
  bin. The sample-rate check is now done in kHz with the spectrum
  (`R/frequencyStats.R:87`) and does fire: a spectrum with bins above the wave's
  Nyquist raises "Frequency is greater than half sample rate". `output.unit`
  conversion is applied to frequencies and bandwidths and not to Q.
* **The dB thresholds and the squaring.** `y <- y^2` at :77 is applied once, and
  the thresholds 0.5 and 0.1 are then half-power and tenth-power, which is the
  right pairing. (The `>=` in the `rle()` at :188 versus the `>` in
  `above <- x[y > level]` at :213 is a genuine inconsistency, but a bin whose
  squared amplitude is exactly the threshold is not constructible in double
  precision, so it is unreachable and not reported above.)
* **`fcis` per-bin indices**, against hand-built amplitude matrices:
  `power` is `rowMeans(amp^2)`; `entropy` is `-sum(p log p)/log(n_frames)` with
  `p` normalised per bin and non-finite terms dropped, matching a hand
  computation to 6 digits and giving exactly 1 for a bin that is constant over
  time; `ACI` matches the hand sum; `background` matches
  `stats::quantile(type = 7)` for every row; `cover` is the fraction of frames
  above three times the background. The matrix-over-vector recycling in
  `amp / rowSums(amp)` and `amp > 3*bg` divides and compares per bin, which is
  what is wanted. All-zero bins and single-frame windows return 0 rather than
  `NaN`.
* **`ste()`** — `e[i] <- cs[i - before + U + 1] - cs[i - before]` sums exactly the
  `U+1` samples centred on `i`: a unit click at sample 50 with `U=10` gives
  non-zero energy at samples 45-55 and nowhere else, and an all-ones wave gives
  `e[50] == 11`. The guard at :39 correctly returns all zeros when the wave is
  shorter than a window.
* **`zerocross()`** — a 1 kHz tone sampled at 8 kHz for 1 s gives 2000 crossings
  and a 997 Hz tone gives 1994. A run of exact zeros reports one crossing at the
  first zero, not one per zero (`c(1,1,1,1,1,0,0,0,0,0,-1,...)` → crossings at 6
  and 16), and a DC offset smaller than the amplitude does not add spurious
  crossings.
* **`jitter()` and `shimmer()`** — the definitions and divisors match the standard
  local measures: `sum(|diff(T)|)/(N-1)` over `N` periods, relative jitter
  divided by the mean period, shimmer as the mean `|20*log10(A[i+1]/A[i])|` over
  `N-1` pairs. A square wave of exact period 80 samples and a phase-shifted sine
  of the same period both give exactly zero jitter; a single doubling of
  amplitude across 100 periods gives 0.0614 dB against the predicted
  `20*log10(2)/99 = 0.0608`. The residual jitter reported for a sine that starts
  at a sample of exactly zero is the floating-point sign of `sin(k*pi)`, which the
  documentation at `R/jitter.R:8-10` already covers.
* **`generateTimeMask()`** — `on`/`off` are derived so the period is never shorter
  than `wl/n.periods`, the mask is truncated rather than recycled short, and the
  measured duty cycle is 0.95 for `n.periods` of 10, and 0.944/0.946 for 7 and 3
  where the period does not divide the wave. Both channels of a stereo wave and
  all columns of a `WaveMC` are masked identically and correctly (the
  matrix-times-vector recycling at :64 is per sample, not per channel).
* **`generateTimeShift()`** directions and rotation: `where="start"` on
  `1:10` at 10 Hz with `amount=0.3` gives `0 0 0 1 2 3 4 5 6 7`, `where="end"`
  gives `4 5 6 7 8 9 10 0 0 0`, `rotate` gives `8 9 10 1 2 3 4 5 6 7`, and a
  rotation by the whole length is the identity. Shifts longer than the wave give
  all silence rather than an error.
* **`windowing()`** window arithmetic. Starts, counts, final-window handling and
  gap filling are all right: `n=1000, wl=300, ov=0` gives 3 complete windows
  (starts 1, 301, 601) and 4 with `complete.windows=FALSE` (the last 100 samples
  long); `ov=100` gives 4 and 5; a negative overlap gives the intended gaps; a
  wave of zero samples gives no windows; and `bind.wave=TRUE` with a gap
  reassembles `1:20` exactly.
* **`sweptsine()`** phase and endpoints. Both modes start at `f0` and reach `f1`
  at the last sample (measured 4000 Hz for a 1k-4k sweep in both linear and log
  mode), the phase is the integral of the instantaneous frequency in each case,
  and the waveform is continuous (max adjacent difference 0.0285 for a 100-200 Hz
  sweep).
* **`normalise()`** does keep `bit` and `pcm` consistent with the data. Reading
  tuneR's `normalize` source: `unit` is matched against
  `c("1","8","16","24","32","64","0")` and `pcm` defaults to `object@pcm`, so
  `as.character(wave@bit)` is always a legal unit and the format is preserved.
  Measured round trips: 16-bit PCM → peak 32767, 8-bit → peak 254, 24-bit →
  8388607, 32-bit PCM → 2147483647, 32-bit float → peak 1 with `pcm` still
  `FALSE`, 64-bit float → peak 1. `WaveMC` works too (tuneR has a `WaveMC`
  method for `normalize`). I initially suspected `bit=1` waves would come back
  mislabelled, but tuneR's `postWaveform()` converts `bit=1` to `bit=32,
  pcm=FALSE` before returning, so no reachable wave has `bit=1`.
  Coverage note: there is no `tests/testthat/test-normalise.R` at all, so this is
  untested — it just happens to be right.
* **`allChannels()` and `WaveFilter`** channel handling and ordering. One entry
  per channel for mono (1), stereo (2) and a 3-channel `WaveMC` (3), in channel
  order, for both the `channel.param=NULL` extraction path and the
  `channel.param` path. `.filterAllChannels()` recombines in the right order
  (left stays left, right stays right; `WaveMC` column means come back
  11, 211, 411 for inputs with means 5.5, 105.5, 205.5 doubled) and restores
  `samp.rate`, `bit` and `pcm` from the input.
* **`concat()`** (`R/Wave-methods.R`) — binds `Wave` and `WaveMC` correctly,
  rejects a mixture of the two classes, and `.noClick()` trims the end of the
  first wave, the start of the last, and both ends of the ones in between, which
  is the right pattern. tuneR's `equalWave()` catches a mono/stereo mixture
  before `bind()` can silently drop a channel.
* **`corWaveMC()`** sign convention. Reading real seewave's `corenv`:
  `offsett = (offsetp - n)/f` is exact for a delay greater than zero and one
  sample high for a delay of zero or less, and `delays = -o$t` then means
  "arrived later than channel 1", which is what `R/corWaveMC.R:6-13` claims and
  what `tdoa()`/`bearing()` expect. The precomputed `reference` is the same
  object every channel is correlated against, and `plot=FALSE` is passed so
  `corenv`'s default plotting is suppressed.
* **`specStats()`, `addSpectra()`, `subtractSpectra()`, `zeroSpectrum()`** —
  bin-for-bin arithmetic, `validateComparableSpectra()` enforcement, the
  `coerceNegative` default and the `sd`-needs-two-spectra check all behave as
  documented.
* **`PseudoWave` seeding** — `.withSeed()` registers its `on.exit` on the right
  frame: the caller's `.Random.seed` is restored exactly, a fixed seed is
  reproducible, and `seed=NA` draws afresh each time.
* **`frequency.R`** — `frequencySound()`, `naturalFrequency()` and
  `resonantFrequency()`; the damped-oscillator discriminant and the
  overdamped error are right, and `resonantFrequency()` is `naturalFrequency()`
  at zero resistance.
* **`disp.R`** — DISP chunk parsing: the four-byte format number, the
  short-chunk and empty-item cases, single-bracket lookup so an undefined format
  number gives `NA` rather than an error, and the three text formats with their
  encodings.
* **`sheepFrequencyStats.R`** is a data documentation stub with no code.

## Left unverified

The `seewave` stand-in means the following were reasoned about but not measured,
and are not reported as findings: the absolute scaling of `meanspec()` output
(window gain compensation, one- versus two-sided scaling), and therefore whether
`frequencyStats()`'s squared amplitudes are proportional to power as its -3 dB
and -10 dB labels assume; `seewave::sfm()`'s own treatment of zero bins; and
`spectro()`'s behaviour when a window is shorter than `wl` (reading its source,
`step <- seq(1, n+1-wl, ...)` looks as though it would raise "wrong sign in 'by'
argument" for `n < wl`, which `fcis(wave, window.length = w, wl = 256)` with
`w < 256` would reach). Running this area's tests against the real `seewave` and
`tuneR` would settle these.
