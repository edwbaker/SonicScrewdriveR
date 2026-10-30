# Correctness review: web services, Python bridge, classes and polar plots

Area reviewed: `R/audioblast.R`, `R/audioblastProcessors.R`, `R/xenoCanto.R`, `R/birdNet.R`,
`R/birdnetReticulate.R`, `R/googleSpeech.R`, `R/reticulate-virtualenv.R`, `R/scikit-maad*.R`,
`R/exponential_backoff.R`, `R/defaultCluster.R`, `R/packageManagement.R`, `R/conditions.R`,
`R/Classes.R`, `R/TimeRegion.R`, `R/utils.R`, `R/utils-plot.R`, `R/utils-deprecated.R`,
`R/radar.R`, `R/radialPolygon.R`, `R/dielPlot.R`, `R/dielHistogram.R`, `R/directionPlot.R`,
`R/yearlyPlot.R`.

Correctness only. 30 findings, most severe first.

## Issues filed

Filed in `edwbaker/SonicScrewdriveR`, all labelled `bug`. Findings not listed
here were judged too marginal to open an issue for, or are folded into the issue
of a finding they share a cause with.

| Issue | Finding |
|---|---|
| [#124](https://github.com/edwbaker/SonicScrewdriveR/issues/124) | 1 — `req_retry(retry_on_failure=)` vs the declared httr2 minimum |
| [#125](https://github.com/edwbaker/SonicScrewdriveR/issues/125) | 2 — `directionPlot()` drops directions above 180° |
| [#129](https://github.com/edwbaker/SonicScrewdriveR/issues/129) | 3, 22 — `package.installed()` installs unasked; `pythonInstall()` cancel |
| [#130](https://github.com/edwbaker/SonicScrewdriveR/issues/130) | 9, 9a — `last_page` termination and the string case |
| [#134](https://github.com/edwbaker/SonicScrewdriveR/issues/134) | 8 — `rbind` column mismatch across pages and batches |
| [#135](https://github.com/edwbaker/SonicScrewdriveR/issues/135) | 5 — xeno-canto am/pm comments lose date and time |
| [#139](https://github.com/edwbaker/SonicScrewdriveR/issues/139) | 20 — xeno-canto empty field shifts later fields |
| [#141](https://github.com/edwbaker/SonicScrewdriveR/issues/141) | 4 — `dielHistogram()` bars drawn from a hard-coded radius |
| [#144](https://github.com/edwbaker/SonicScrewdriveR/issues/144) | 11, 21 — right-closed bins; empty `times` crash |
| [#147](https://github.com/edwbaker/SonicScrewdriveR/issues/147) | 12 — `radialPolygon()` narrow sector not drawn |
| [#150](https://github.com/edwbaker/SonicScrewdriveR/issues/150) | 10 — `yearlyFraction()` takes the year from `year=` |
| [#153](https://github.com/edwbaker/SonicScrewdriveR/issues/153) | 13, 24 — `yearlyPositions("days")` off by one; `.isLeapYear()` not vectorised |
| [#158](https://github.com/edwbaker/SonicScrewdriveR/issues/158) | 15, 16 — `TimeRegion` one sample too long; no `validity` method |
| [#159](https://github.com/edwbaker/SonicScrewdriveR/issues/159) | 6 — `maad_spectrogram(mode=)` never reaches Python |
| [#164](https://github.com/edwbaker/SonicScrewdriveR/issues/164) | 7 — `.spectrogram_maad_power()` ignores `@mode` |
| [#168](https://github.com/edwbaker/SonicScrewdriveR/issues/168) | 17, 18 — `fn` passed as a list; `maad$util` never imported |
| [#170](https://github.com/edwbaker/SonicScrewdriveR/issues/170) | 19 — unnormalised samples sent to scikit-maad |
| [#173](https://github.com/edwbaker/SonicScrewdriveR/issues/173) | 23 — `defaultCluster()` sets no RNG stream |
| [#176](https://github.com/edwbaker/SonicScrewdriveR/issues/176) | 25 — `readBirdNet()` column layout, timestamp `NA`, empty directory |
| [#177](https://github.com/edwbaker/SonicScrewdriveR/issues/177) | 29 — `filter = NA` dropped; `ab_range(NA)`; `mime = NA` → `.NA` |

Finding 14 (`emptyDiel()` has no `plotrix` guard) and finding 26 (`alt` is `NA`
at the poles) were added as a [comment on #70](https://github.com/edwbaker/SonicScrewdriveR/issues/70#issuecomment-6530804355),
which already covers `dielPlot(method=)` and would be fixed in the same lines.

Not filed: findings 27 and 28 (a single filter value containing a comma; the
non-ASCII escape under `LC_ALL=C`), both PLAUSIBLE and conditional on behaviour
I could not verify here — audioBlast's own comma handling, and whether users meet
the C locale. They are recorded above if either is worth confirming later.

## How this was verified

CRAN is unreachable from this container and building cloned CRAN sources is blocked, so the
package could not be installed and `testthat` could not be run against it. Instead the real
dependencies that matter for this area were installed as Debian binaries — **`jsonlite` 1.8.8,
`httr2` 1.0.0, `plotrix` 3.8.4, `ggplot2` 3.4.4, `stringi`, `digest`, `mime`, `hms`, `Rdpack`,
`reticulate` with `numpy` 1.26.4** — and the area's source files were sourced directly and driven
with constructed fake responses (`httr2::with_mocked_responses`) and captured `polygon()` calls.
No live call was made to audioBLAST, Xeno-canto or any Google API.

Three Imports could not be obtained and were replaced by my own minimal stand-ins: **`tuneR`**
(a plain S4 `Wave` class with the same `left`/`right`/`stereo`/`samp.rate` slots), **`seewave`**
and **`suncalc`**. Only finding 15 touches one of those, and only through the slots named above;
its arithmetic is independent of `tuneR`. No finding below depends on `seewave` or `suncalc`
behaviour. `scikit-maad` could not be installed, so findings 7, 8, 18 and 19 are marked
accordingly.

`CONFIRMED` means I reproduced the stated wrong output. `PLAUSIBLE` means read-only reasoning,
or that the R-side half was reproduced but the remote/Python side could not be.

## What was checked and found correct

Worth recording, because these were the brief's main suspicions:

- **audioBlast pagination is sound *when the response carries a numeric `last_page`*.** Driven
  with 0, 1, 10 (exactly one full page), 11 and 25 records at a page size of 10: every page is
  fetched exactly once, no page is dropped or duplicated, 25 of 25 ids arrive with no gaps and no
  duplicates, `max_pages` truncates correctly, a non-default start page works, and a start page
  past the end returns `NULL`. `last_page` is read from the response, not assumed. Every response
  in audioBlast's own saved fixtures carries it as a number — but the loop's termination rests
  entirely on that, which is findings 9 and 9a.
- **`dielHistogram` bin edges are exact.** `seq(0, 2*pi, by = 2*pi/(1440/minutes))` lands on
  `2*pi` to the bit for `hour`, `30minute` and `15minute`; 24/48/96 bins, no spurious 24th or
  25th bin, and midnight falls in bin 1. (But see finding 11 for the other edge.)
- **`radialPolygon` angle handling is right.** Zero points along +x, `reverse=TRUE` runs
  clockwise, and a sector that wraps past midnight closes correctly: `radialPolygon(2*pi-0.2,
  0.2, 1, 2)` draws an arc spanning 0.4 rad, not its 6.08 rad complement, and the complementary
  call spans 6.27 rad. `angle1 == angle2` draws the whole circle. (But see finding 12.)
- **`exponential_backoff()` grows and is capped.** The default schedule is
  1 1 2 2 3 4 5 6 8 10 s, saturating at `last`; 42 s over ten attempts. Its one caller,
  `gs_transcribe_execute()`, does terminate and does `stop()` on exhaustion, so an exhausted
  loop cannot be mistaken for success.
- **No status that will never succeed is retried.** `.audioblast_perform()` leaves httr2's
  default `is_transient` in place, which retries 429 and 503 only; a 400 or 404 is raised at
  once, with audioBlast's own explanation extracted from the body.
- **Taxon names with spaces and brackets are encoded correctly.**
  `taxon = "Gryllus Gryllus campestris"` is sent as `?taxon=Gryllus%20Gryllus%20campestris`,
  and `"Gryllus (Gryllus) campestris"` as `...%20%28Gryllus%29%20...`.
- **The deprecated-argument shims forward correctly, with the right order.** `dielFraction`,
  `yearlyFraction`, `convert2radians` and `region` each give identical results under the old
  and new names, including an old name by keyword followed by positional arguments
  (`dielFraction(input="seconds", 3600, "fraction")`), an abbreviated old name (`region(uni=)`),
  forwarding through another function's dots (`sapply(x, convert2radians, input="degrees")`),
  and a clear error when both names are given. `region("seconds",1,2)` and
  `region(unit="seconds",1,2)` produce identical objects.
- **Missing and null JSON fields parse correctly.** A `null` value, and a field present in only
  one row of a page, both become `NA` in the right column. A single-record page comes back as a
  one-row data frame, not unlisted. Numeric-as-string columns are converted, and a column
  holding one non-numeric value is correctly left as character.
- **R matrices do reach Python as `ndarray`.** So passing `object@Sxx` without `np_array()` in
  `maad_frequency_entropy()` and `maad_spectral_activity()` is not a defect.
- **Python exceptions surface as usable R errors**
  (`Error: ValueError: invalid literal for int() ...`).
- **`as.integer(Nt)` reaches Python as `int`**, an R double as `float`, and `NULL` as `None`.

---

## Findings

### 1. `R/audioblast.R:376` and `R/audioblast.R:511` — every audioBlast request fails on the minimum declared `httr2`

`req_retry(retry_on_failure=)` was added in httr2 1.1.0, but `DESCRIPTION` declares
`httr2 (>= 1.0.0)`, so the whole web API surface is broken on a permitted dependency version.

**Failure scenario.** With httr2 1.0.0 installed (as it is in this container),
`audioblast("data", "recordings", taxon = "Gryllotalpa vineae")` →
`Error: unused argument (retry_on_failure = TRUE)`. Same for `audioblastRecord()`,
`audioblastFilters()`, `audioblastDownload()` (via `.audioblast_download`, line 376) and
anything else that reaches `.audioblast_perform()`. The error is raised inside `audioblast()`'s
`tryCatch` and handed to `on.issue`, so the user sees that bare message with no hint that a
dependency is too old. `args(httr2::req_retry)` in 1.0.0 is
`(req, max_tries, max_seconds, is_transient, backoff, after)` — no `retry_on_failure`.

**CONFIRMED.** Reproduced against the real httr2 1.0.0; the rest of this review needed
`req_retry` patched to get past it. `tests/testthat/helper-audioblast.R:126` uses the same
argument, so the suite dies the same way rather than reporting a version problem.

### 2. `R/directionPlot.R:17` — every direction above 180° is silently dropped

`convert2radians()` maps degrees onto `[0, 2*pi)`, but the x scale is
`scale_x_continuous(limits = c(-pi, pi))`, which removes anything above `pi`. Half the compass
cannot be plotted.

**Failure scenario.** `directionPlot(c(10, 90, 181, 270, 350), c(1, 2, 3, 4, 5))` builds a plot
whose segment layer holds x values `0.1745 1.5708 NA NA NA` — only the 10° and 90° segments are
drawn. The three westerly directions vanish **with no warning at all**, so nothing tells the
caller that 3 of 5 observations are missing from the figure.

**CONFIRMED** against real ggplot2 3.4.4.

**Coverage gap.** `tests/testthat/test-directionPlot.R` does exercise this — "the plot can be
drawn" builds `directionPlot(c(0, 90, 180, 270), c(1, 2, 3, 4))` — but only asserts
`expect_no_error()`. The 270° row is dropped silently, so the assertion passes, and the
neighbouring `expect_no_warning` test uses only 0/90/180. No test asserts how many segments
are actually drawn.

### 3. `R/packageManagement.R:8-14` — the package's only optional-dependency guard installs packages unasked and reports success on failure

`package.installed()` is the sole guard for every Suggests dependency; `requireNamespace()` is
used nowhere in the package. Three defects at once:

- `askYesNo()` returns its `default`, which is `TRUE`, when there is no one to answer. So in a
  non-interactive session the guard goes straight on to `install.packages()`.
- It `return(TRUE)` after `install.packages()` without checking whether the install worked.
- The callers' own helpful messages are therefore unreachable: `package.installed()` either
  installs or `stop()`s with `"Missing package not installed."` before the caller can say which
  package and why.

**Failure scenario.** `Rscript -e 'sonicscrewdriver::dielPlot(Sys.Date(), 51, 0)'` on a machine
without `plotrix` prints `Operation requires missing package plotrix. Install now?`, receives no
answer, and **calls `install.packages("plotrix")`** — an unrequested write to the user's library
from a plotting function. Where the install then fails (no network, read-only library) the
function returns `TRUE`, the guard at `R/dielPlot.R:169` passes, and the call dies at
`plotrix::radial.plot` with `there is no package called 'plotrix'`. The same path is taken by
`getMaad()`, `birdNetAnalyse()`, `pythonInstall()`, `gs_transcribe()`, `emptyYearly()` and
`yearlyPlot()`, so `R CMD check`, CI and cron runs all attempt installs. Reproduced directly:
`package.installed("nosuchpkg")` under `Rscript` printed the prompt and proceeded to
`install.packages`, returning `TRUE` after the download failed.

**CONFIRMED.**

### 4. `R/dielHistogram.R:41` — bar bases are hard-coded at radius 1, so bars escape the plot

The bar height is scaled by `diff(limits)` at line 30, but the call draws from the literal `1`
rather than from `limits[1]`.

**Failure scenario.** `dielPlot()`'s own default is `limits = c(0, 2)`, so a histogram drawn on
a default diel plot with `dielHistogram(times, limits = c(0, 2))` scales counts over a span of
2 and then draws them from radius 1: the fullest bin runs from r=1 to **r=3**, outside the
`radial.lim = c(0, 1, 2)` that `emptyDiel()` sets. With `limits = c(0, 1)` bars run 1→2, again
outside. Captured radii: `limits=c(1,2)` → 1→2 (correct by coincidence); `limits=c(0,2)` → 1→3;
`limits=c(0,1)` → 1→2.

**CONFIRMED.**

**Coverage gap.** `test-dielHistogram.R` tests `limits=c(1, 3)` but only asserts on the returned
`data$value`, which is correct; it never checks the radii passed to `radialPolygon()`.

### 5. `R/xenoCanto.R:82-83` — am/pm comments lose both their time and their date

Fields 4's time and date are split on the literal `"h,"`. The collection writes `06:30h,` for a
24-hour clock but `2:00 pm,` for an am/pm one — no `h`. Neither `sub()` matches, so `time` and
`date` are both set to the whole field.

**Failure scenario.**
`.mdXenoCanto("XC2 © R // Oslo (59.9, 10.7), Norway // 120 m // 2:00 pm, 2019-05-01 // call")`
gives `time = "2:00 pm, 2019-05-01"` and `date = "2:00 pm, 2019-05-01"`. In
`.mdNormaliseXenoCanto()` the anchored `^[0-9]{4}-[0-9]{2}-[0-9]{2}$` test then fails, so
`datetime` is `NULL` — the recording loses its date as well as its time. The same comment
written as `06:30h, 2019-05-01` yields `datetime = 2019-05-01 06:30:00`. `.mdXcClock()` reads
`"2:00 pm"` correctly as `14:00:00`, so the careful am/pm handling the file documents at length
("158 carried am or pm, and 30 of those ... a reader that ignored the marker would place twelve
hours early") never receives a readable time. By the file's own figures this is ~158 of 15,834
comments.

**CONFIRMED.**

### 6. `R/scikit-maad-sound.R:18,28` — `maad_spectrogram()`'s documented `mode` is never sent to Python

`mode` is a named formal, so `...` cannot carry it, and the call to
`maad$sound$spectrogram()` omits it. It is only written into the `@mode` slot.

**Failure scenario.** `maad_spectrogram(wave, mode = "amplitude")` computes scikit-maad's
default **power** spectrogram and labels it `@mode == "amplitude"`. Every downstream index then
treats power values as amplitudes; `mode = "complex"` is equally ignored. Verified by
inspection of the source: `mode` does not appear in the `maad$sound$spectrogram(...)` call.

**CONFIRMED** (the omission; the numerical consequence is PLAUSIBLE, as scikit-maad could not
be installed here).

### 7. `R/scikit-maad-classes.R:5-18` — `.spectrogram_maad_power()` ignores `@mode`, so dB spectrograms get converted to dB twice

`.spectrogram_maad_power()` returns any `spectrogram_maad` unchanged whatever its `@mode`, and
`.spectrogram_maad_dB()` then applies `10 * log10()` to it. The `@mode` slot exists to record
this and nothing reads it.

**Failure scenario.** Feed a dB spectrogram to `maad_spectral_activity()`, which calls
`.spectrogram_maad_dB()`: an `Sxx` of `1 10 100 1000` dB comes back as `0 10 20 30`, and a
realistic dB matrix containing `0` and `-3` comes back as `-Inf` and `NaN`, which then poison
`ACTfract`/`ACTcount`/`ACTmean`. Reproduced directly on a `spectrogram_maad` with
`mode = "dB"`. `.spectrogram_maad_dB()` also stamps `mode = "dB"` on its result, so a second
pass through any dB-based index repeats the conversion.

**CONFIRMED.**

### 8. `R/audioblast.R:609` and `R/audioblast.R:138` — a page or batch with a different column set destroys the whole query

`do.call(rbind, rets)` requires identical columns across pages and across id batches.
`jsonlite` fills in missing fields within one page, but not across pages.

**Failure scenario.** A two-page search where page 2 carries one field page 1 did not →
`Error: numbers of columns of arguments do not match`, and both pages are lost. Reproduced.
Line 138 is worse, because id batches are by construction different records and so more likely
to come from different sources: `audioblast("data", "recordings", id = 1:150)` where the second
batch of ids returns one extra field raises the same error, after 150 ids have been fetched.
Reproduced.

**CONFIRMED.**

### 9. `R/audioblast.R:595` — a response without `last_page` has no terminating condition, and the query fails even when something stops it

`min(NULL, Inf)` is `Inf`, so `last` becomes `Inf` and `while (query$page < last)` has no
termination of its own. The empty-result guard above it tests `data` only, not `last_page`, so a
response carrying rows but no page count reaches line 595. A JSON `null` behaves identically,
because `jsonlite` gives `NULL` for both an absent key and a null scalar.

**Failure scenario.** Any 200 response whose body lacks `last_page` — an API change, a different
module shape, or an interposed proxy page — with `max_pages` left at its `NULL` default. Driven
with capped mocks:

| fake API | requests issued | outcome |
|---|---|---|
| 10 pages, then refuses | 11 | **errors**, all 10 pages of rows discarded |
| 40 pages, then refuses | 41 | **errors**, all 40 pages discarded |
| 999 pages | aborted at the 60-request budget | **did not terminate** |
| `last_page: null`, 999 pages | aborted at the 60-request budget | **did not terminate** |
| no `last_page`, `max_pages = 3` | 3 | returns 3 pages |

So nothing inside the loop stops it: it ends only when the API refuses the page past the last,
and because that refusal is raised as an error, **every row already fetched is thrown away**. The
sharpest case is the smallest: a **one-page** result with rows and no `last_page` issues 2
requests and returns an *error* instead of the single row it had already received. Giving
`max_pages` bounds it (`min(NULL, 3)` is `3`), so only the documented default is exposed.

**CONFIRMED.** (Re-verified at a peer session's request with a hard request budget in the mock;
an earlier uncapped run had reached 1668 requests in 6 seconds before I cut it off.)

### 9a. `R/audioblast.R:595` — `last_page` as a JSON string loses most of the result set silently

If `last_page` arrives as a string, `min()` coerces to character and `query$page < last` becomes
a **character** comparison, which stops as soon as the page number sorts above the page count as
text. No error is raised and nothing is logged.

**Failure scenario.** With a 200-page result set:

| `last_page` | pages fetched | with a numeric `last_page` |
|---|---|---|
| `"10"` | **2** | 10 |
| `"20"` | **3** | 20 |
| `"100"` | **2** | 100 |
| `"9"` | 9 | 9 (correct by luck) |
| `"3"` | 3 | 3 (correct by luck) |

`"2" < "10"` is `FALSE`, so a 10-page search returns **2 pages — 20% of the records — and reports
success**. This is the silent record loss the review was looking for: a caller has no way to tell
a truncated result from a complete one. All nine audioBlast fixtures in
`tests/testthat/fixtures/audioblast` carry `last_page` as a JSON number, so the API does not do
this today; the defect is that the code depends on that without checking, and a single
serialisation change on the server would silently truncate every multi-page query. One
`as.numeric()` on line 595 would remove both this and the type dependence.

**CONFIRMED** (the parsing and loop behaviour); **PLAUSIBLE** as a live fault, since it needs
audioBlast to serialise the field as a string.

### 10. `R/yearlyPlot.R:83,102` — `yearlyFraction()` takes the year from its `year` argument, not from `t`

The `year` argument decides the divisor (365 or 366) while `yday` comes from `t`, and `year`
defaults to 2022.

**Failure scenario.** `yearlyFraction(as.POSIXct("2024-12-31"))` returns exactly `2*pi`, i.e.
the angle of 1 January — the last day of a leap year is plotted on top of the first. Every date
after February of a leap year is a day out when the default is used:
`yearlyFraction(as.POSIXct("2024-02-29"))` equals `yearlyFraction(as.POSIXct("2022-03-01"))`
exactly. Passing `year = 2024` fixes it, but nothing in the signature or the call makes that
necessary, and plotting a multi-year dataset cannot pass a single correct `year` at all.

**CONFIRMED.**

### 11. `R/dielHistogram.R:21,25` — bins are right-closed, so a time exactly on the hour is counted in the previous bin

`hist()` defaults to `right = TRUE` with `include.lowest = TRUE`, so `(from, to]` bins are
built from breaks the code labels `from`/`to` in the returned data frame as if they were
`[from, to)`.

**Failure scenario.** `dielHistogram(c(00:00, 01:00, 02:00, 23:00), by = "hour")` puts counts in
bins **1, 1, 2, 23** instead of 1, 2, 3, 24. Scheduled recorders that fire on the hour — the
normal case — have every record attributed to the hour before. Midnight is the lone exception,
landing in bin 1 only because `include.lowest` catches it, so the convention is not even
self-consistent. Reproduced.

**CONFIRMED.**

**Coverage gap.** Every time in `test-dielHistogram.R` is at `:30` or `:45`; no test places a
time on a bin boundary.

### 12. `R/radialPolygon.R:47-48` — a sector narrower than `angleinc` collapses to a two-vertex polygon and is not drawn

`seq(min_a, max_a, by = angleinc)` returns a single element when the span is below `angleinc`,
and line 48 then overwrites it, leaving one angle. `xpos`/`ypos` hold two points, and
`polygon()` draws nothing visible.

**Failure scenario.** `angleinc` defaults to 0.01 rad = 138 s of a day. A 2-minute band gives
`2*pi*120/86400 = 0.0087` rad and **2 vertices**; 137 s also gives 2. `dielPlot()` draws its
sunrise band as `radialPolygon(dielFraction(tim$sunrise), dielFraction(tim$sunriseEnd), ...)`
(`R/dielPlot.R:229`) — the time the sun's disc takes to clear the horizon, about 2 minutes near
the equator — so at low latitudes the sunrise and sunset bands disappear from the plot
altogether, and at mid latitudes (3-4 minutes) they are drawn from 4 vertices. The same applies
to any short `dielRings()` entry. Reproduced by capturing the `polygon()` call.

**CONFIRMED.**

### 13. `R/yearlyPlot.R:32` — `yearlyPositions("days")` is 1-based while everything else is 0-based

`2*pi * (1:YearDays)/YearDays` puts day 1 at `2*pi/365`, but `yearlyPositions("months")[1]` is
`0` and `yearlyFraction(POSIX)` gives `0` for 1 January (it uses `yday`, which is 0-based).
`yearlyPlot()`'s own ring also starts at 0 (`angs <- (0:length(suntime)) * 2*pi/length(suntime)`).

**Failure scenario.** `yearlyPositions(2022, "days")[1]` is `0.017214` where
`yearlyFraction(as.POSIXct("2022-01-01"))` is `0`: day labels drawn with `format = "days"` sit
one day clockwise of the data they label, and the last label lands at `2*pi`, on top of the
first. `yearlyFraction(1, input.unit = "day")` is likewise `0.017214`, so the `"day"` and
`"POSIX"` input units disagree by a day for the same date. Reproduced.

**CONFIRMED.**

### 14. `R/dielPlot.R:93-106` — `emptyDiel()` calls `plotrix` with no guard

`emptyYearly()` checks `package.installed("plotrix")` at `R/yearlyPlot.R:124`; the exported
`emptyDiel()` has no such check and goes straight to `plotrix::radial.plot`.

**Failure scenario.** `emptyDiel()` on a machine without `plotrix` →
`Error: there is no package called 'plotrix'`, naming neither what to install nor why.
Confirmed by inspecting the function body: `package.installed` does not appear in it.
(`dielPlot()` does check, at line 169, but only after it has already called `emptyDiel()`'s
sibling work — and its check has the problems in finding 3.)

**CONFIRMED.**

### 15. `R/TimeRegion.R:55` — a region given in seconds, minutes or hours is one sample too long, and consecutive regions overlap

Both ends are multiplied by the same factor and both are then used as inclusive indices, so a
span of *n* units yields *n* × rate **+ 1** samples, and the end of one region is the start of
the next.

**Failure scenario.** On a 100 Hz wave, `region("seconds", 1, 2)` resolves to samples 100:200 —
**101 samples for a 1-second region** — and `region("seconds", 2, 3)` resolves to 200:300, so
sample 200 is in both. A windowed analysis that steps through a recording a second at a time
double-counts one sample per window and reports each window as 1.01 s long. `region("seconds",
0, 1)` is correct (1:100, because `max(1, 0)` clamps the start), so the first window is right
and every later one is wrong. Reproduced on a 1000-sample wave.

A non-integer bound is also truncated rather than rounded: `region("seconds", 0.015, 0.025)` at
100 Hz gives bounds 1.5 and 2.5, which index as 1:2 — 2 samples for a span of 1.

**CONFIRMED.** The `tuneR` `Wave` class was my own stand-in, but `.timeRegion2samples()` is pure
arithmetic on `@samp.rate` and the subsetting is plain vector indexing.

**Coverage gap.** `tests/testthat/test-TimeRegion.R:45` asserts the off-by-one as correct:
`expect_equal(length(w@left), 44101)` for `region("seconds", 1, 2)` at 44100 Hz. Fixing the
arithmetic requires changing that expectation.

### 16. `R/TimeRegion.R:6-18` — `TimeRegion` has no `validity` method, so invalid regions are accepted

`setClass()` declares slots and a prototype but no `validity`. `region()` validates `input.unit`
and nothing else, and `new("TimeRegion", ...)` bypasses even that.

**Failure scenario.** `new("TimeRegion", from = 10, to = 1, unit = "seconds")` passes
`validObject()`, and `new("TimeRegion", from = 1, to = 2, unit = "furlongs")` does too. A
reversed region survives until `[` reports `"Time region begins after the end of the wave."` —
which is false and points at the wave rather than the region. `region("seconds", 10, 1)` is
accepted for the same reason. A negative `from` is also accepted.

**CONFIRMED.**

### 17. `R/scikit-maad-utils.R:5,9` — `maad_power2dB()` reads a submodule `getMaad()` does not attach

`getMaad()` attaches `maad$sound` and `maad$features` explicitly (`R/scikit-maad.R:17-18`),
which is only necessary because importing `maad` does not bring its submodules with it.
`maad_power2dB()` then reads `maad$util`, which is never attached.

**Failure scenario.** `maad_power2dB(x)` with the module from `getMaad()` →
`AttributeError: module 'maad' has no attribute 'util'`. The function is also not exported in
`NAMESPACE`, so this is reachable only as `sonicscrewdriver:::maad_power2dB()`, which limits the
blast radius.

**PLAUSIBLE** — the omission is confirmed by inspection; the `AttributeError` could not be
reproduced because `scikit-maad` is not installable here.

### 18. `R/scikit-maad-indices.R:230-237` — `maad_number_of_peaks()` passes `fn` as a Python list, the bug `maad_spectral_entropy()` documents fixing

`object@fn` is handed over raw. `maad_spectral_entropy()` wraps the same value in
`np_array()` at line 279, with a comment saying exactly why: *"fn has to reach Python as an
array rather than as a list. Left as a list, scikit-maad's own default for flim, which is
(fn.min(), fn.max()), has no min() to call."*

**Failure scenario.** Verified against real `reticulate` and `numpy` 1.26.4: an R numeric vector
arrives in Python as a **`list`**, and `hasattr(list, "min")` is `FALSE`, while
`np_array(...)` arrives as `ndarray` with `min` present. So
`maad_number_of_peaks(spectrogram)` hands scikit-maad a list where it expects an array, and
`slopes`/`prominence` are wrapped for that very reason two lines below.

**PLAUSIBLE** — the R-to-Python conversion is CONFIRMED; the resulting scikit-maad error could
not be reproduced without the package.

### 19. `R/scikit-maad-sound.R:35-40` — samples are sent to scikit-maad unnormalised, and extra channels are dropped

`maad_wave()` passes `wave@left` (or column 1 of a `WaveMC`) straight through. A `tuneR` `Wave`
holds integer samples — ±32767 at 16 bit — where scikit-maad expects floats in [-1, 1]. The
source acknowledges it: `#ToDo: Process wave to be expected as maad.`

**Failure scenario.** Every dB-valued result is offset by roughly `20*log10(32768)` ≈ 90 dB, so
the documented defaults `dB_threshold = 3` (`maad_temporal_activity`, `maad_temporal_events`)
and `dB_threshold = 6` (`maad_spectral_activity`) are thresholds about 90 dB below any real
signal, and `ACTfrac`/`ACTcount`/`EVTfrac` come back saturated at 1 regardless of the
recording. `maad_temporal_median()` returns a value in the tens of thousands. Separately, a
stereo `Wave` or any `WaveMC` is analysed on one channel with no warning that the others were
discarded.

**PLAUSIBLE** (`scikit-maad` not installable here).

### 20. `R/xenoCanto.R:50` — an empty field shifts every later field up one

`parts <- parts[nzchar(parts)]` drops empty fields, but fields 2-4 are then read by position.

**Failure scenario.**
`.mdXenoCanto("XC5 © R // Oslo, Norway //  // 06:30h, 2019-05-01 // song")` — a comment
with nothing between the elevation separators — yields
`stated_elevation = "06:30h, 2019-05-01"`, `time = "song"` and `type = NULL`. The time, date and
vocalisation type are all lost, and a time string is reported as an elevation. The file's own
notes say the placeholders `?m`, `-m` and `m` occur, which `.mdXcNonEmpty()` handles; a
genuinely empty field is not handled. Reproduced.

**CONFIRMED.**

### 21. `R/dielHistogram.R:27-28` — an empty `times` crashes with an unrelated message

`max(h$counts)` over an empty histogram makes `maxval` degenerate, and the `data$value[i] == 0`
test at line 39 is then `NA`.

**Failure scenario.** `dielHistogram(as.POSIXct(character(0)))` →
`Error: missing value where TRUE/FALSE needed`, which names neither the argument nor the
problem. Filtering a dataset down to no detections in a window is a routine way to reach this.
Reproduced.

**CONFIRMED.**

### 22. `R/reticulate-virtualenv.R:20` — cancelling the install prompt gives an internal error instead of the message

`askYesNo()` returns `NA` when the user cancels, and `if (!NA)` is an error.

**Failure scenario.** `pythonInstall()` in an interactive session, user presses cancel →
`Error: missing value where TRUE/FALSE needed`, instead of the intended
`"The 'sonicscrewdriver' environment is required to use maad functions in sonicscrewdriver."`
Reproduced.

Separately, and more worryingly, `askYesNo()` returns its `TRUE` default when nothing answers:
in a session where `interactive()` is true but stdin never replies, `pythonInstall()` proceeded
to build a virtualenv and begin a multi-gigabyte `pip install` of tensorflow unprompted — I hit
this accidentally while testing and had to kill it. `package.installed()` (finding 3) has the
same root cause with no `interactive()` guard at all.

**CONFIRMED.**

### 23. `R/defaultCluster.R:22-33` — no RNG stream is set, so parallel results are not reproducible

`makeCluster`/`makeForkCluster` are used without `clusterSetRNGStream()`, and the function has
no teardown of its own.

**Failure scenario.** Two consecutive `defaultCluster(FALSE)` clusters drew
`0.649392 0.232178 0.859757` and `0.631975 0.350592 0.316531` for the same code — so any
windowed analysis using `jitter`, `generateNoise` or a randomised filter gives different
answers on every run, and `set.seed()` before `defaultCluster()` does not help. Reproduced on a
4-core machine (3 workers).

The absence of teardown is documented in the examples (`cl <- defaultCluster(); stopCluster(cl)`),
so a leak when the caller's body errors is the caller's responsibility, not this function's.

**CONFIRMED.**

### 24. `R/yearlyPlot.R:50-64` — `.isLeapYear()` is not vectorised

Written as nested `if`s on `year`.

**Failure scenario.** `.isLeapYear(c(2023, 2024))` →
`Error: the condition has length > 1`, so `yearlyPositions(year = c(2022, 2024))` and
`yearlyFraction(t, year = years_vector)` fail rather than returning one value per year. `year`
is a plain argument with no documented length restriction. Reproduced.

**CONFIRMED.**

### 25. `R/birdNet.R:21` — a BirdNET results file in another column layout fails with a message about rows

`ret$\`Begin.Time..s.\` <- as.numeric(ret$\`Begin.Time..s.\`)` assigns `numeric(0)` when the
column is absent.

**Failure scenario.** A file whose header is `Selection<TAB>Start (s)<TAB>End (s)` →
`Error: replacement has 0 rows, data has 1`, which says nothing about BirdNET columns.
Reproduced.

Two smaller problems in the same function: `readBirdNet()` on an empty directory returns `NULL`
silently (`do.call(rbind, list())`), and `filename_parsing = "timestamp"` on a filename that is
not a Unix timestamp sets every `Start`/`End` to `NA` with only an `NAs introduced by coercion`
warning — reproduced on `20190101_123000.BirdNET.results.txt`, which `"audiomoth"` parses
correctly as `2019-01-01 12:30:00` and `"timestamp"` turns into `NA`.

**CONFIRMED.**

### 26. `R/dielPlot.R:212` — `alt` is `NA` where the sun does not reach solar noon

`alt` comes from `getSunlightPosition(tim$solarNoon, ...)`, and `if (alt <= -0.314159)` is an
error when that is `NA`.

**Failure scenario.** A polar date where `suncalc` gives `NA` for `solarNoon`, with
`tim$night` also `NA`, reaches `if (NA <= -0.314159)` →
`Error: missing value where TRUE/FALSE needed`. Confirmed that `if (NA <= -0.314159)` raises
that error; whether `suncalc` returns `NA` for `solarNoon` at the relevant latitudes I could
not check, as `suncalc` is not installable here.

**PLAUSIBLE.**

### 27. `R/audioblast.R:503-507` — a single filter value containing a comma is sent comma-joined, contradicting the comment above it

The comment at line 502 says *"A value with a comma in it can only be sent in the list form"*,
but the guard is `length(v) > 1 && any(grepl(","...))`, so a single such value takes the
`paste(v, collapse=",")` branch.

**Failure scenario.** `audioblast("data", "recordings", author = "Reynolds, W. J.")` sends
`?author=Reynolds%2C%20W.%20J.`, which decodes to `Reynolds, W. J.` — indistinguishable from
two values. Given `c("Reynolds, W. J.", "Ragge")` the same filter is correctly sent as
`?author[]=Reynolds%2C%20W.%20J.&author[]=Ragge`, so one author matches and two do not.
Reproduced the URLs; whether audioBlast splits on the decoded comma I did not probe.

**PLAUSIBLE.**

### 28. `R/audioblast.R:503-508` — non-ASCII filter values are mangled in a C locale

Filter values reach `httr2::req_url_query()` in whatever encoding the session is in, and
`audioblast()` never calls `.useUtf8Locale()` or `enc2utf8()`.

**Failure scenario.** Under `LC_ALL=C`, `taxon = "Cañada niño"` is sent as
`?taxon=Ca%3Cc3%3E%3Cb1%3Eada%20ni%3Cc3%3E%3Cb1%3Eo`, i.e. the literal text
`Ca<c3><b1>ada ni<c3><b1>o`, and matches nothing. Under `C.UTF-8` the same value is correctly
sent as `?taxon=Ca%C3%B1ada%20ni%C3%B1o`. Both reproduced. The package's own
`.useUtf8Locale()` documentation names exactly where the C locale turns up — *"cron jobs,
containers, continuous integration and `R CMD check`"* — but calls it only from the Python entry
points. `enc2utf8()` on filter values would give the correct escape in either locale (verified).

**PLAUSIBLE** (CONFIRMED under `LC_ALL=C`; whether users hit that locale is environmental).

### 29. Smaller confirmed defects in `R/audioblast.R`

- **`R/audioblast.R:124` — `max_pages` is applied per id batch, not per query.**
  `audioblast("data", "recordings", id = 1:250, max_pages = 1)` issues **3** data requests and
  returns 3 pages, because the 250 ids split into 3 batches that each paginate independently.
  Documented as *"Maximum number of data pages to return"*. Reproduced.
- **`R/audioblast.R:405-417` — a row whose `mime` is `NA` is saved with a `.NA` extension.**
  `names(map)[map == NA]` is all-`NA` rather than empty, so `length(e) == 0` is false and
  `paste0(".", e[1])` gives `".NA"`: a recording from `xc` with id 1 and no mime is written as
  `xc_1.NA`. Reproduced. An unrecognised mime correctly gives no extension.
- **`R/audioblast.R:254` — `ab_range(NA, 20)` fails with
  `Error: missing value where TRUE/FALSE needed`** from `min > max`, rather than saying the
  bound is missing. Reproduced.
- **`R/audioblast.R:629` — a filter given `NA` is silently dropped rather than refused.**
  `.audioblast_filters(list(taxon = NA, source = "x"))` returns only `source`, so
  `audioblast("data", "recordings", taxon = NA)` returns everything in the module instead of
  nothing or an error. Reproduced.
- **`R/audioblast.R:144` — any warning raised inside the query discards every record already
  fetched.** The `tryCatch` handler routes warnings to `on.issue`, which defaults to `stop`.
  Reproduced with a warning raised during the transport: a query that had already collected its
  rows returned none. `on.issue` is documented as handling *"error or warning"*, so this is the
  stated design — but it means one benign warning on page 40 of 40 loses all 40 pages, with no
  way to ask for the records anyway.
