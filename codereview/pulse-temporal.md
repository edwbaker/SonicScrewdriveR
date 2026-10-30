# Correctness review: pulse detection, beat and rhythm, duty cycle, rainfall

Area reviewed: `R/pulse.R`, `R/pulseDetection.R`, `R/pulseDetection_dietrich.R`,
`R/pulseDetection_simple.R`, `R/pulseDetection_threshold.R`, `R/pulseIntervals.R`,
`R/pulseStats.R`, `R/beatSpectrum.R`, `R/beatComplexity.R`, `R/dutyCycle.R`,
`R/rainfallDetection.R`, `R/rainfall_bedoya2017.R`.

## How this was verified

R 4.3.3 was installed from Ubuntu's archive. CRAN is blocked by this session's network
policy (`cloud.r-project.org:443` → 403 at the agent proxy), building cloned CRAN sources
was refused by the sandbox, and Debian ships no `r-cran-tuner`, `r-cran-seewave`,
`r-cran-waveletcomp` or `r-cran-signal` (1130 `r-cran-*` packages, none of those four), so
the package's testthat suite could not be run.

Instead the package's own files (`validationFunctions.R`, `ste.R`, and the detector,
interval, stats, duty-cycle and beat-complexity files under review) were sourced
unmodified against a `Wave` S4 stub. The code under review touches only `@left`,
`@samp.rate`, `@bit` and `length()`; tuneR defines `Wave` with slots
`left`/`right`/`stereo` over `WaveGeneral`, and `length(Wave)` as `length(x@left)`
(`tuneR/R/Wave.R:7`, `tuneR/R/length.R:1`), which the stub reproduces exactly. Results
were checked to be identical at float (±1), ±10000 and ±32767 amplitudes and with
integer-rounded sine bursts, so nothing below depends on the sample scale.

`pulseIntervals()`, `pulseStats()` and `beatComplexity()`'s peak logic take plain
lists/data frames, so no stub is in their path. Findings that *do* depend on a stand-in
say so and are marked PLAUSIBLE: `rainfall_bedoya2017()` was checked by replaying
`seewave::meanspec()`/`stdft()`'s own arithmetic from seewave's source in base R rather
than by calling seewave, and `beatSpectrum()` was checked only up to the
`analyze.wavelet()` call. `pulse()` needs `tuneR::silence()` and was reviewed by reading
only; no finding is raised against it.

Synthetic square and sine-burst waves with a known, exact number of pulses at known
positions were the primary instrument, as asked.

---

## 1. `pulseIntervals()` can never flag an interval for 6 or fewer pulses — CONFIRMED

`R/pulseIntervals.R:26`

An interval is kept only when `diffs > m + nsd*s`, but in a sample of *k* values the
largest attainable z-score is `(k-1)/sqrt(k)`, which first exceeds the default `nsd=2` at
*k* = 6 gaps (7 pulses). For any shorter pulse train the test is arithmetically
unsatisfiable, so the function returns an empty result however extreme the gap.

Failure scenario: `pulseIntervals(list(onsets=c(100,200,300,1000,1100,1200)))` — six
pulses with one gap seven times longer than the rest. Gaps are 100,100,700,100,100;
mean 220, sd 268.33, cutoff 756.66 > 700. Returns `list(onsets=numeric(0),
offsets=numeric(0))`. Measured maxima: k=2 → 0.707, k=3 → 1.155, k=4 → 1.500,
k=5 → 1.789, all below 2.

Test coverage hides this: `tests/testthat/test-pulseIntervals.R` only ever calls with
`nsd=1` or `nsd=10`; the default `nsd=2` is never exercised.

## 2. A pulse already in progress at the first analysed sample makes `pd_dietrich2004()` mis-pair every later pulse — CONFIRMED

`R/pulseDetection_dietrich.R:79` (`candidates <- which(runs$values & starts > 1)`)

The onset scan discards a run that begins at the first analysed sample, but the offset
scan still reports that pulse's end. The result has one more offset than onset, and
`pulseStats()` pairs positionally, so every pulse is matched to the previous pulse's
offset.

Failure scenario: `n=8000`, square pulses at samples 1–400, 3001–3400 and 5001–5400
(three pulses, each 400 samples). `pd_dietrich2004()` returns `onsets = c(2955, 4955)`
and `offsets = c(450, 3450, 5450)`; `pulseStats()` then reports
`durations = c(-2505, -1505)` where the truth is `c(400, 400, 400)`. Reproduced with an
integer-rounded 200 Hz sine burst at samples 1–400 as well: 0 onsets, 1 offset (451).

`tests/testthat/test-pulseStats.R` covers one more onset than offset but never the
reverse, which is exactly this case.

## 3. `pd_threshold()` debounces onsets and offsets independently, so survivors belong to different pulses — CONFIRMED

`R/pulseDetection_threshold.R:31-32`

`.pd_threshold_debounce()` is applied to the onset list and the offset list in two
separate passes. Because the spacing between onsets differs from the spacing between
offsets whenever pulses have unequal durations, the two passes keep different pulses.

Failure scenario: `n=1000`, pulses at 101 (10 samples), 121 (40 samples), 171
(10 samples), `threshold=0.5`, `U=30`. Ground truth with `U=1` is
`onsets = c(101,121,171)`, `offsets = c(111,161,181)`. With `U=30` the function returns
`onsets = c(101,171)` but `offsets = c(111,161)` — the 171 onset is paired with the 121
pulse's offset — and `pulseStats()` reports `durations = c(10, -10)`.

The existing test (`test-pulseDetection.R`, "pd_threshold debounces onsets within U
samples") asserts only `all(diff(few$onsets) > 200)` and never checks that onsets and
offsets stay paired.

## 4. `pd_simple()` reports a spurious whole-wave pulse on audio containing no pulses — CONFIRMED

`R/pulseDetection_simple.R:26` (`theta <- min(e_u) + gamma*(max(e_u) - min(e_u))`)

`ste()` returns zeros for the first `floor(U/2)` and last `ceiling(U/2)` samples
(`R/ste.R:38-45`), so `min(e_u)` is 0 for any wave longer than the window, whatever the
real noise floor. `theta` therefore collapses to `gamma*max(e_u)` and can never rise to
the noise floor, while `F_a = theta + alpha/scaling*e_v` is only about 0.22 of the signal
itself when `V = 4U`. Any stationary signal clears it.

Failure scenario: `mkwave(rnorm(8000, sd=0.01))` — stationary Gaussian noise with zero
pulses. `pd_simple()` returns `onsets = 60`, `offsets = 7940` and flags 7880 of 8000
samples as pulse. `theta` is 0.0586 while the true minimum short-term energy away from
the padded edges is 0.751 — thirteen times higher. The reported boundaries 60 and 7940 are
exactly `floor(U/2)` and `n - floor(U/2)`, i.e. artefacts of the zero padding, not of the
signal. An all-ones wave gives the same 60/7940 pair.

This also means a pulse genuinely starting at sample 1 is reported 59 samples late: pulse
1–400 in `n=8000` gives `onsets = 60`, and a pulse running to the last sample
(7601–8000) is reported as ending at 7940.

No test builds a pulse-free non-silent wave, or a pulse at the very start or end, so the
gap is uncovered.

## 5. A wave entirely above threshold yields zero pulses from `pd_dietrich2004()` — CONFIRMED

`R/pulseDetection_dietrich.R:40-41`

When `e_u >= F_1` holds across the whole analysed range, `rle()` gives a single run
starting at position 1, which `starts > 1` excludes; the offset scan's run is all
`FALSE`, so it has no candidates either.

Failure scenario: `mkwave(rep(1, 8000))` returns `onsets = integer(0)` and
`offsets = integer(0)`. On the same input `pd_simple()` reports one pulse spanning
60–7940, so the package's two implementations of the same cited algorithm disagree on
whether the wave contains a pulse. Likewise on stationary noise: `pd_dietrich2004()`
returns nothing, `pd_simple()` returns a whole-wave pulse.

## 6. The rainfall SNR is computed on a time-averaged spectrum, not per time frame, so any broadband noise reads as rain — CONFIRMED (statistic), PLAUSIBLE (deviation from Bedoya)

`R/rainfall_bedoya2017.R:2, 11-13`

`seewave::meanspec()` collapses the STFT with `rowMeans()` before anything else
(`seewave/R/seewave.r:4456-4459`), so `mean(a[,2])/sd(a[,2])` is a mean and sd taken
across the band's frequency bins of a *time-averaged* spectrum. The cited method's
implementation (hardRain, which states it follows Bedoya et al. 2017) computes
`mean(z)/sd(z)` across the band's bins **within each time frame** and classifies frames
(`hardRain/R/getMetrics.r`: `s2n <- sapply(x, function(y) apply(y, 2, function(z)
mean(z)/sd(z)))`). Averaging over time first removes the temporal variance that the
statistic is meant to measure, which inflates the ratio.

Failure scenario (base-R replication of seewave's own `2*Mod(fft(x*w))/wl` → `rowMeans`
→ square arithmetic, 1 s of `rnorm(44100, sd=0.05)`, `wl=512`): the coded time-averaged
ratio is **17.47**, the per-frame median that Bedoya/hardRain compute is **1.33** — a
13× difference across the `Tsnr=3.5` decision boundary. Over 200 independent draws of
plain broadband noise, **100%** exceeded `Tsnr=3.5` (mean 11.16, sd 4.51, range
4.55–32.22), i.e. undifferentiated noise is always classified as rainfall. The package's
own test encodes this: `test-rainfallDetection.R` asserts
`expect_gt(rainfallDetection(rain), 0)` where "rain" is `rnorm(n)*15000`.

The band itself is right: `meanspec()` returns frequency in kHz
(`seewave/R/seewave.r:4463`, `/1000`), so `p[,1] > 0.6 & p[,1] < 1.2` is 600–1200 Hz,
matching the cited band. The deviation is in the statistic, not the band.

Marked PLAUSIBLE for the claim about Bedoya et al. specifically: the paper is paywalled
and this session's network resolves only GitHub (`eprints.lancs.ac.uk`, `rdrr.io` and
`oparu.uni-ulm.de` all fail to resolve), so the comparison rests on hardRain's source and
documentation rather than the paper itself. Those hosts would need allowing in the
session's network settings to close it.

## 7. `Tmean = 1e-6` is compared against an un-normalised PSD, so the first decision criterion is always true — CONFIRMED

`R/rainfall_bedoya2017.R:1, 14`

`meanspec(..., norm=FALSE)` passes `scale=FALSE` to `stdft()`, which returns
`2*Mod(fft)/wl` with no normalisation (`seewave/R/seewave.r:9107-9118`). The magnitude
therefore scales with the input's amplitude units, so a fixed absolute threshold has no
consistent meaning across recordings.

Failure scenario (same replication): one second of `rnorm(sd=0.05)` gives
`mean_a = 2.41e-05` at float scale, `2.59e+04` at 16-bit PCM scale and `1.11e+14` at
32-bit — nine orders of magnitude apart, and all vastly above `1e-6`. The
`mean_a > Tmean` test cannot fail for any non-silent recording, so the decision rests
entirely on the SNR test of finding 6.

## 8. A threshold crossing through a sample exactly equal to the threshold is lost, giving an offset with no onset — CONFIRMED

`R/pulseDetection_threshold.R:21-24`

`above <- mag > threshold` and `below <- mag < threshold` are both strict, so a sample
exactly at the threshold is in neither set and a crossing that passes through such a
sample is detected in neither direction.

Failure scenario: `mkwave(c(rep(0,10), rep(0.5,10), rep(1,10), rep(0,10)))` with
`threshold=0.5`. The threshold resolves to exactly 0.5, the 0.5 plateau is neither above
nor below, and the function returns `onsets = numeric(0)` with `offsets = 31` — one
pulse, zero onsets, one offset. `pulseStats()` then reports no durations at all. With
`pd=TRUE` on the same wave the comparison happens in energy and the onset reappears
(`onsets = 21`), so the two modes disagree about how many pulses the wave contains.

## 9. A single `NA` sample makes `pd_threshold()` silently report no pulses — CONFIRMED

`R/pulseDetection_threshold.R:10`

`max(mag)` is `NA` without `na.rm`, so `threshold` is `NA`, every comparison is `NA`, and
`which()` drops them all. There is no warning.

Failure scenario: `mkwave(c(rep(0,50), NA, rep(1,20), rep(0,50)))` contains one obvious
pulse. `pd_threshold(w, threshold=0.5)` returns `onsets = numeric(0)`,
`offsets = numeric(0)` — a clean "no pulses" answer for a wave that has one.

`dutyCycle()` has the same exposure at `R/dutyCycle.R:38` but fails loudly instead:
`dutyCycle(mkwave(c(rep(1,500), NA, numeric(499))), normalise=FALSE)` raises
`missing value where TRUE/FALSE needed` from inside `validateDutyCycle()`, which does not
mention `NA` or the wave.

## 10. `psi` cannot change `pd_dietrich2004()`'s result — CONFIRMED (inert), PLAUSIBLE (deviation from the cited state machine)

`R/pulseDetection_dietrich.R:77, 84-85`

`psi` enters only as `cycle <- max(2, floor(psi) + 2)`, which marks every `cycle`-th
sample of a run untestable. The scan then fires at the *first* testable sample above
`F_a` (`hit[1]`), which is `k = 2` for every `cycle >= 3`. So `psi` has no effect, and
insofar as it has any it works backwards: a larger `psi` leaves *more* samples testable,
which would make detection laxer rather than stricter.

Failure scenario: `n=8000`, pulses at 1001–1400 and 3001–3400. `psi = 1, 2, 5, 20, 100,
400` all return `onsets = c(955, 2955)` and `offsets = c(1450, 3450)`; only `psi = 0`
differs (`onsets = c(956, 2956)`). A documented tuning parameter spanning three orders of
magnitude changes nothing.

In Dietrich et al. (2004) ψ is a persistence requirement — the number of consecutive
samples that must exceed `F_a` before a boundary is declared — so raising it should make
detection stricter. That half is PLAUSIBLE: the paper is paywalled and unreachable from
this session (see finding 6), and `inst/REFERENCES.bib:8` gives only the citation, with no
parameter description. `tests/testthat/test-pulseDetection.R` pins the behaviour to a
`ref_dietrich()` copy of the previous loop, so it verifies fidelity to the old code rather
than to the paper and would not notice.

## 11. Both energy detectors overstate pulse durations by about `U`, and merge pulses closer together than `U` — CONFIRMED

`R/pulseDetection_simple.R:24, 29` and `R/pulseDetection_dietrich.R:24, 40-41`

`ste()` sums `abs()` over a centred window of `U+1` samples, so the energy begins rising
`U/2` samples before a pulse and decays `U/2` samples after it. The threshold is crossed
part way up each ramp and nothing compensates, so both boundaries are biased outward.

Failure scenario: a single square pulse occupying samples 1001–1400 (duration exactly
400) in `n=8000` at default `U=120`, `V=480`:

| detector | onset | offset | duration | error |
|---|---|---|---|---|
| `pd_simple` | 954 | 1446 | 492 | +92 |
| `pd_dietrich2004` | 955 | 1450 | 495 | +95 |
| `pd_threshold` | 1001 | 1401 | 400 | 0 |

Two pulses separated by a single sample (1001–1400 and 1402–1801) are merged into one by
both energy detectors: `pd_simple` returns `onsets = 954`, `offsets = 1847`, and
`pd_dietrich2004` returns `onsets = 955`, `offsets = 1851` — a count of 1 where the truth
is 2. More seriously, five 400-sample pulses separated by 100-sample gaps are reported as
**one** pulse by `pd_dietrich2004` (`pd_simple` and `pd_threshold` both get 5).

The existing test allows `abs(res$onsets - c(2001, 6001)) <= 240`
(`test-pulseDetection_simple.R`), a tolerance of two window lengths, which papers over the
systematic bias.

## 12. `dutyCycle()`'s threshold is relative to the wave's peak, so one transient collapses the measurement — CONFIRMED

`R/dutyCycle.R:38` (`c <- sum(w > limit*max(w))`)

`limit` is documented as "Threshold above which to consider the signal", but it is applied
as a fraction of the peak of the whole wave. A single loud sample rescales the threshold
for every other sample.

Failure scenario: `c(rep(0.3, 100), numeric(899), click)` in 1000 samples — a genuine
duty cycle of 0.1. With `click = 0` the function returns 0.1000; with `click = 10` it
returns **0.0010**, a hundredfold error, for a wave whose signal-bearing portion is
unchanged.

The same line makes `normalise` a no-op: dividing by `max(w)` is scale-invariant, so
`dutyCycle(w, normalise=TRUE)` and `dutyCycle(w, normalise=FALSE)` are identical, and
scaling a wave by 1000 does not change the answer. The default `normalise = TRUE` costs a
full copy of the wave and buys nothing.

(The duty cycle itself cannot exceed 1 — `c <= l` always — and `validateDutyCycle()` is a
correct belt-and-braces check. `percent` output is `100 *` the unit value, consistent.
`test-dutyCycle.R` pins the peak-relative behaviour via the `2/3` sine assertion, so it is
intended; the transient sensitivity above is the undocumented consequence.)

## 13. `pulseIntervals()` returns intervals that start at the preceding pulse's onset, so the reported "area of no pulses" contains a pulse — CONFIRMED

`R/pulseIntervals.R:36-37`

The documented purpose is "to locate area of no pulses", but the interval is reported as
`onsets[i]` → `onsets[i+1]`. The silence actually begins at `offsets[i]`, so the whole of
pulse *i* is inside the reported gap. `pulses$offsets` is never read.

Failure scenario: 13 pulses each 50 samples long at onsets
`c(100,200,…,1000, 5000,5100,5200)`, giving one 4000-sample gap.
`pulseIntervals()` returns `onsets = 1000`, `offsets = 5000`; the pulse starting at 1000
runs to 1050, so the first 50 samples of the reported "no pulses" region are a pulse.
For long pulses the error is the full pulse duration.

## 14. `pulseIntervals()` flags unusually *short* gaps as areas of no pulses — CONFIRMED

`R/pulseIntervals.R:26` (`odds <- diffs > m + nsd*s | diffs < m - nsd*s`)

The two-sided outlier test treats an unusually short inter-onset interval as a gap worth
reporting, which is the opposite of an area of no pulses.

Failure scenario: onsets `c(100, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000,
16010, 18000, 20000, 22000)` (pulses 50 samples long) — one pair 10 samples apart among
2000-sample gaps. `pulseIntervals()` returns `onsets = 16000`, `offsets = 16010`: a
10-sample "interval of no pulses" that lies entirely inside the pulse occupying
16000–16050.

## 15. `beatComplexity()` returns `-Inf` when the beat spectrum has no strict local maximum — CONFIRMED

`R/beatComplexity.R:34`

`max(bs$period[peaks])` on an empty vector gives `-Inf` with a bare
`no non-missing arguments to max` warning, and `-Inf / 0` is returned as the complexity.

Failure scenario (with `beatSpectrum()` stubbed to return a known spectrum, so the peak
logic is the real code): monotonically rising power returns
`list(complexity = -Inf, peak.periods = numeric(0), num.peaks = 0)`. A spectrum of
length 1 or 2 does the same.

Separately, `e <- c(0, diff(sign(diff(power))), 0)` with `peaks <- e == -2` requires a
strict rise then a strict fall, so a flat-topped maximum is missed entirely: power
`c(1,2,10,10,2,1)` has an obvious peak but yields `num.peaks = 0` and
`complexity = -Inf`. (The indexing itself is right: `e[i]` aligns with `period[i]`.)

## 16. `beatSpectrum()`'s returned periods do not honour `min_period` or `max_period`, and the excess propagates into `beatComplexity()` — CONFIRMED

`R/beatSpectrum.R:42, 45-46`

`lowerPeriod` is fixed at `2^1` steps of the resampled signal, i.e. `2*min_period`
seconds, and `upperPeriod` is `2^ceiling(1 + log2(max_period/min_period))` steps, which
overshoots `max_period` by between 2× and 3.3×.

Failure scenario (arithmetic on the function's own expressions):

| `min_period` | `max_period` | returned period range | overshoot |
|---|---|---|---|
| 0.005 | 30 | 0.0100 – 81.920 s | low ×2, high ×2.73 |
| 0.01 | 10 | 0.0200 – 20.480 s | low ×2, high ×2.05 |
| 0.002 | 5 | 0.0040 – 16.384 s | low ×2, high ×3.28 |

The lag-to-period conversion itself is correct — `analyze.wavelet()` is called on a data
frame with no `dt`, so `wt$Period` is in resampled steps and `* min_period` converts to
seconds, and `tests/testthat/test-beatSpectrum.R` already asserts
`min(bs$period) == 2*0.005`. The correctness consequence is downstream:
`beatComplexity()`'s `complexity` is `max(bs$period[peaks]) / num.peaks`
(`R/beatComplexity.R:34`), so with the defaults it can be driven by a peak at a period of
up to 81.9 s when the caller asked for no more than 30 s. PLAUSIBLE for the
`analyze.wavelet()` half — WaveletComp could not be installed, so the period vector it
actually returns was not observed.

## 17. `pulseStats()` pairs onsets and offsets positionally with no ordering check — CONFIRMED

`R/pulseStats.R:5`

`offsets[seq_len(paired)] - onsets[seq_len(paired)]` assumes `offsets[i]` belongs to
`onsets[i]`. Nothing checks that the offset follows its onset, so misaligned detector
output becomes a negative duration rather than an error or a warning.

Failure scenario: `pulseStats(list(onsets=c(500,1500), offsets=c(100,900,1900)))` returns
`durations = c(-400, -600)`. This is what turns findings 2 and 3 from "the wrong boundary"
into silently negative physical quantities, in a package whose own description says it
"validates that values are physically possible wherever feasible".

## 18. `pd_threshold()`'s threshold is relative to the whole wave's peak, hiding quiet pulses — CONFIRMED

`R/pulseDetection_threshold.R:10`

`threshold <- threshold * max(mag)` is a fraction of the global peak, with no absolute
option, and `pd_threshold()` carries no roxygen block at all, so neither this nor `pd`'s
effect on the units is documented.

Failure scenario: `c(numeric(100), rep(0.1,50), numeric(100), rep(1.0,50), numeric(700))`
contains two unambiguous pulses. With the default `threshold = 0.2` the threshold is 0.2,
and the function returns `onsets = 251`, `offsets = 301` — one pulse, the quiet one
discarded.

`pd = TRUE` compounds it: the threshold is then a fraction of peak *power*, so
`threshold = 0.5` means 0.707 of peak amplitude rather than 0.5. The same `threshold`
value selects a different amplitude in the two modes (see the disagreement in finding 8).

## 19. `pd_simple()`'s onsets and offsets use conventions that differ by one sample from each other and from `pd_threshold()` — CONFIRMED

`R/pulseDetection_simple.R:41-42`

`which(diff(pulse) == 1)` is the index of the last sample *below* threshold, while
`which(diff(pulse) == -1)` is the index of the last sample *above* it. `pd_threshold()`
uses first-above and first-below instead.

Failure scenario: `n=8000`, pulses at 1001–1400 and 3001–3400. `pd_simple()` reports
`onsets = c(954, 2954)` while the first above-threshold samples in its own returned
`pulse` vector are `c(955, 2955)`. Durations happen to come out right because the two
off-by-ones cancel, but any caller using the onset alone — to cut a pulse out of the
wave, say — is one sample early, and onsets are not comparable between methods.

## 20. A zero-length wave gives `pd_simple()` and `pd_dietrich2004()` a `NaN` threshold and bare `min`/`max` warnings — CONFIRMED

`R/pulseDetection_simple.R:26` and `R/pulseDetection_dietrich.R:26`

With `ste()` returning a zero-length vector, `min(e_u)` is `Inf` and `max(e_u)` is `-Inf`,
so `theta` is `Inf + gamma*(-Inf - Inf)` = `NaN`.

Failure scenario: `pd_simple(mkwave(numeric(0)))` and
`pd_dietrich2004(mkwave(numeric(0)))` both emit
`no non-missing arguments to min; returning Inf` and return `theta = NaN`.
`pd_threshold()` handles the same input cleanly (`R/pulseDetection_threshold.R:12-17`).
A single-sample wave is handled correctly by all three, except that `pd_threshold()`
reports `onsets = 1` for it.

## 21. `beatSpectrum()` and `beatComplexity()` silently use the left channel only, and fail on `WaveMC` — CONFIRMED (the error), PLAUSIBLE (the stereo result)

`R/beatSpectrum.R:33, 36`

`dutyCycle()`, `rainfallDetection()` and `pulseDetection()` all route multi-channel input
through `allChannels()`; `beatSpectrum()` reads `wave_std@left` directly and does not.

Failure scenario: a stereo `Wave` whose channels differ returns a single spectrum computed
from the left channel, with nothing in the return value saying so. A `WaveMC` raises
`no slot of name "left" for this object of class "WaveMC"` — tuneR's `WaveMC` has no
`left` slot (`tuneR/R/Wave.R:13`), so this is certain from the class definition;
`beatComplexity()` inherits both.

## 22. `beatSpectrum()` collapses the envelope to one or two points for short waves — CONFIRMED (the collapse), PLAUSIBLE (the consequence)

`R/beatSpectrum.R:34-39`

`runmed(k)` only warns and shrinks `k` when `k > n`, and `t1 <- seq(0, t0[n], by=min_period)`
yields almost no points when the wave is shorter than a few `min_period`s.

Failure scenario (the function's own lines 33–39 replayed verbatim, at 44100 Hz with the
default `min_period = 0.005`, giving `runmed_k = 221`):

| samples | duration | rows handed to `analyze.wavelet()` |
|---|---|---|
| 100 | 0.0022 s | 1 (plus `'k' is bigger than 'n'! Changing 'k' to 99`) |
| 221 | 0.0050 s | 1 |
| 441 | 0.0100 s | 2 |

A wave under ~0.01 s is reduced to a one- or two-point series before the wavelet
transform, with only a `runmed` warning to show for it. PLAUSIBLE for what
`analyze.wavelet()` then does, since WaveletComp could not be installed.

## 23. The rainfall detector uses one of the cited method's two bands, and `meanspec(PSD=TRUE)` squares after averaging — PLAUSIBLE (band count), CONFIRMED (the squaring)

`R/rainfall_bedoya2017.R:2-3`

hardRain, which states it follows Bedoya et al. (2017), requires **both** 0.6–1.2 kHz and
4.4–5.6 kHz to clear their thresholds (`hardRain/R/getMetrics.r`:
`freqLo = c(0.6, 4.4), freqHi = c(1.2, 5.6)`; `classifyRain()` combines the bands). This
code tests 600–1200 Hz alone, which removes the cross-band agreement that suppresses
false positives from biophony in that band — the same single-band weakness that
finding 6 measures. hardRain also passes `wn = "rectangle"` to keep the PSD unmodified,
where `meanspec()`'s default `wn = "hanning"` applies here; and hardRain's own worked
thresholds are around 1.88 for s2n, not 3.5.

Separately, `meanspec(PSD=TRUE)` squares *after* `rowMeans()`
(`seewave/R/seewave.r:4459-4466`), so the quantity is `(mean_t |X|)^2`, not the mean
power `mean_t(|X|^2)` that a PSD is. Measured on 1 s of `rnorm(sd=0.05)`: `2.27e-05`
as coded versus `2.81e-05` for the true mean power, 24% low, with a bias that varies
with how non-stationary the recording is.

Two smaller points in the same function, both PLAUSIBLE (seewave not installable):
`wl = 512` is hardcoded, so at 44.1 kHz the band holds only 7 bins and `sd` over 7 values
is very unstable (over 200 identical-distribution noise draws the ratio ranged 4.55–32.22,
sd 4.51); and `step <- seq(1, n+1-wl, ...)` inside `meanspec()` takes a negative upper
bound for a wave shorter than 512 samples, which should raise
`wrong sign in 'by' argument` from inside seewave rather than a message from this
function. The `nrow(a) < 2` guard at `R/rainfall_bedoya2017.R:7` is correct and well
placed; a perfectly flat band still gives `sd = 0` and `c = Inf`, which passes `Tsnr`.

---

## Not findings

- `pulse()` (`R/pulse.R`) — reviewed by reading only. The `bit == 8` handling is right:
  `tuneR::postWaveform()` offsets 8-bit channels by +127 (`tuneR/R/Wave.R`), so silence
  sits at the midpoint and `invert` writing 0 is a correct full-scale negative pulse. The
  `seq_len(pulse.length)` guard and the `.waveFullScale()` comment both hold up. The only
  nit is that the `leading + pulse.length > duration` check consults `pulse.length` for
  `type="dirac"`, which ignores it.
- `pulseDetection()` (`R/pulseDetection.R`) — dispatch, `allChannels()` routing and
  `.validateChoice()` are correct; a bad method name on multi-channel input still errors.
- `ste()`'s cumulative-sum rewrite is arithmetically correct for the window it documents;
  the problem in finding 4 is the zero-padded edges leaking into `min(e_u)` downstream,
  not the sums.
- `validateDutyCycle()` correctly bounds the result to [0, 1], and the duty cycle cannot
  exceed 1.
