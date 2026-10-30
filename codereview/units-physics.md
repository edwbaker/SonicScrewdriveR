# Correctness review: units, physics, time

Area reviewed (and nothing outside it):

`R/convertAngles.R`, `R/convertFileSizes.R`, `R/convertPressures.R`,
`R/convertTemperatures.R`, `R/convertTime.R`, `R/audio_filesize.R`, `R/bearing.R`,
`R/dolbear.R`, `R/soundSpeed.R`, `R/wavelength.R`, `R/referenceLevels.R`,
`R/typicalVolume.R`, `R/utils-units.R`, `R/validationFunctions.R`, `R/tdoa.R`,
`R/sDuration.R`, `R/time.R`, `R/dayPhase.R`, `R/STP.R`.

## How this was verified

R 4.3.3 was installed in the container. CRAN and every CRAN mirror are blocked by
the network policy, and Ubuntu noble ships no `r-cran-tuner`, `r-cran-seewave` or
`r-cran-suncalc`, so **`tuneR`, `seewave` and `suncalc` could not be installed**.
`testthat`, `hms`, `Rdpack`, `ggplot2`, `stringi`, `httr2`, `jsonlite`, `digest`,
`mime` and `withr` were installed from the Ubuntu archive.

Everything under review except `dayPhase.R` and the `envelope` path of `tdoa.R`
was exercised directly by sourcing the files, and the package's own test files
for this area (`test-convertAngles.R`, `test-convertPressures.R`,
`test-convertTime.R`, `test-convertFileSizes.R`, `test-audio_filesize.R`,
`test-bearing.R`, `test-dolbear.R`, `test-soundSpeed.R`, `test-wavelength.R`,
`test-referenceLevels.R`, `test-typicalVolume.R`, `test-validationFunctions.R`,
`test-sDuration.R`, `test-dayPhase.R`, `test-unit-arguments.R`) were run against
those sourced definitions. They pass; every failure seen was a missing dependency
or a function outside this area.

To exercise `dayPhase.R` at all, `suncalc` was replaced by a **hand-written port
of the SunCalc algorithm** (the same algorithm the `suncalc` package implements).
It agrees with published times to within a minute or two (London 2024-12-21:
sunrise 08:05, sunset 15:54; Tromsø 2024-06-20: all events `NA`). Findings whose
evidence passed through that stub are labelled accordingly: where the defect
itself is base-R arithmetic the label is CONFIRMED, and the stub is only used to
show the downstream consequence.

Checked and found correct, for the record: every unit round trip
(degrees/radians, Pa/kPa/dyne cm⁻², K/C, bits/bytes/kB/KiB) returns its input
exactly; `.pressureMultipliers`, `.angleMultipliers`, `.wavelengthMultipliers`
are self-consistent; kB = 1000 and KiB = 1024 consistently across
`convert2bytes()`, `humanBytes()` and `audio_filesize()`; Cramer (1993) is
transcribed correctly, including the sign of the `C3` group, and reproduces NPL
reference values (20 °C, 101.325 kPa, dry: 343.36 m/s; 100 % RH: 344.61 m/s);
`bearing()` recovers the true azimuth and elevation to ~1e-13 degrees at every
angle tried in 2-D and 3-D, including across the ±180 discontinuity, and its
ambiguous-array and cone cases are right; `.gcc()` gets the sign and the integer
lag right for both methods at every delay tried, and `.parabolicPeak()`
implements the standard vertex formula (its sub-sample bias on a realistic
2–20 kHz broadband pulse peaks at about 0.2 samples, 4 µs at 48 kHz, which is not
worth reporting); `dayPhase()` covers a recording contiguously with no gaps at
the equator, at 50.1 °N in both solstices, at Tromsø and Svalbard in polar day and
polar night, at both poles, and over a seven-day recording.

---

## Findings, most severe first

### 1. `dayPhase()` silently returns hundreds of thousands of nonsense rows when given a `Date` — `R/dayPhase.R:35`

`etime <- time + duration` adds `duration` as **days** when `time` is a `Date`,
and the later `dt$Start < etime` then compares POSIXct seconds against Date days.
The result is a warning, not an error, so the wrong answer is returned.

Failure scenario: `dayPhase(time=as.Date("2024-06-15"), duration=40000)` returns a
data frame of **431,290 rows** spanning back to 2023, with `Start` and `End`
columns that disagree by months, and only `Warning: Incompatible methods
("Ops.POSIXt", "Ops.Date") for "<="`. The same call with
`as.POSIXct("2024-06-15 00:00:00", tz="UTC")` returns the correct 6 rows. A `Date`
is an easy thing to pass here because the two sibling functions in the same file,
`dayPhases()` and the exported `daysPhases()`, both take one. `time` is never
type-checked. **CONFIRMED** (the failure is base-R arithmetic; the row count was
measured with the stubbed `suncalc`).

### 2. `.startOfDay()` returns an instant in the wrong day on every daylight-saving transition — `R/dayPhase.R:336`

`as.POSIXct(paste(date, "12:00:00"), tz=tz) - 43200` subtracts twelve *real* hours
from local noon. On a day that is 23 or 25 hours long there are not twelve real
hours between midnight and noon, so the result is an hour out in the direction of
the transition. The function's own comment claims this construction handles a
skipped midnight; it does not handle that case either.

Failure scenario, pure base R:

| tz | date | `.startOfDay()` | true first instant | error |
|---|---|---|---|---|
| Europe/London | 2024-03-31 | 2024-03-30 **23:00 GMT** | 2024-03-31 00:00 GMT | −1 h, lands in the previous day |
| Europe/London | 2024-10-27 | 2024-10-27 **01:00 BST** | 2024-10-27 00:00 BST | +1 h, first hour of the day lost |
| America/New_York | 2024-03-10 | 2024-03-09 **23:00 EST** | 2024-03-10 00:00 EST | −1 h |
| America/Havana | 2024-03-10 | 2024-03-09 **23:00 CST** | 2024-03-10 01:00 CDT | −2 h (the skipped-midnight case the comment claims to fix) |

`dayPhases()` uses the result as the window that decides which solar events
"belong" to the day (`R/dayPhase.R:98-106`), so events near local midnight are
attributed to the wrong date. Sweeping latitude and longitude over six transition
dates with the stubbed `suncalc` produced 138 mis-assigned combinations, e.g.
`dayPhases(as.Date("2024-03-31"), lat=20, lon=100, tz="Europe/London")` reports
`Sunrise` at 2024-03-**30** 23:15 and `Day` at 2024-03-30 23:17, where the correct
answer for the 31st is `NA` for both. **CONFIRMED.**

Coverage gap that hides this: `test-dayPhase.R` calls `dayPhases()` on
2024-06-15, 2024-12-15, 2024-04-11 and 2024-06-20 only; no test uses a DST
transition date, and no test asserts anything about `.startOfDay()`.

### 3. A single `NA` aborts every range validator with R's internal message — `R/validationFunctions.R:50,53`

`any(v < min)` is `NA` when any element is `NA`, and `!is.null(min) && NA` is `NA`,
so `if (...)` raises `missing value where TRUE/FALSE needed`. Every validator
built on `.validateNumericRange()` is affected: `validateRH`, `validateKelvin`,
`validateFreq`, `validateWavelength`, `validateDensity`, `validateBulkModulus`,
`validateQ`, `validateDutyCycle`. `validateTimeInSeconds()`, a few lines below at
`R/validationFunctions.R:231`, guards for `NA` explicitly, so the two halves of
the same file disagree about what an unknown value means.

Failure scenario: `convert2Kelvin(c(20, NA, 25), input.unit="C")` →
`Error: missing value where TRUE/FALSE needed`, where the documented contract
("The numeric value in Kelvin", vectorised — the function's own roxygen at
`R/validationFunctions.R:32` advertises that a vector is accepted) implies
`c(293.15, NA, 298.15)`. Likewise `soundSpeed(method="cramer", temp=NA)` and
`validateRH(NA_real_)`. A batch of recordings with one missing temperature kills
the whole call with a message that names neither the argument nor the function.
**CONFIRMED.**

### 4. `validateSpeed()` enforces no bounds, so a negative speed of sound passes — `R/validationFunctions.R:70-72`

`.validateNumericRange(b, "Speed")` is called with neither `min` nor `max`, so it
only checks that the value is numeric. This is the one validator in the file with
no range at all, and `bearing()` at `R/bearing.R:123` is its only caller.

Failure scenario: `bearing(c(0, -1/343, 0), rbind(c(0,0), c(1,0), c(0,1)),
speed=-343)` returns `azimuth = -180` — the exact reverse of the correct `0` — with
no warning. `speed=0` returns `NaN`. For the DESCRIPTION's claim that the package
"validates that values are physically possible wherever feasible", a speed of
sound is about as feasible a check as there is. **CONFIRMED.**

### 5. `wavelength()` validates neither its frequency nor its result — `R/wavelength.R:24-27`

The package defines `validateFreq()` and `validateWavelength()` for exactly this,
and `wavelength()` routes through neither; it hands `speed/frequency` straight to
`.convertLinear()`.

Failure scenario: `wavelength(0)` returns `Inf`; `wavelength(-1000)` returns
`-0.343`, a negative wavelength, which `validateWavelength()` exists to reject
(`R/validationFunctions.R:74-76`). Both then propagate silently into anything
downstream. **CONFIRMED.**

### 6. Cramer's method does not validate pressure, and divides by it — `R/soundSpeed.R:123-141`

`RH` is checked (`R/soundSpeed.R:129`) but `pressure` is not, and
`R/soundSpeed.R:141` divides by it. Nothing checks that the resulting water-vapour
mole fraction is physical either, nor that pressure is inside the 75–102 kPa range
the function's own `@details` states.

Failure scenario: `soundSpeed(method="cramer", temp=20, pressure=0)` returns `NaN`
with no error; a negative pressure is accepted and returns a number. The package's
own documented example, `soundSpeed(method="cramer", temp=14, pressure=3, RH=10)`
(`R/soundSpeed.R:34`), is 3 kPa — roughly the pressure at 24 km altitude — and
returns 342.65 m/s as though nothing were unusual. **CONFIRMED.**

### 7. `cleanTZ()` inverts the sign of a UTC offset — `R/time.R:3`

`paste0("Etc/GMT", substring(tz,4))` maps `"UTC+1"` to `"Etc/GMT+1"`. The POSIX
`Etc/GMT±N` zones are deliberately sign-reversed: `Etc/GMT+1` is UTC**−**1.

Failure scenario: `cleanTZ("UTC+1")` returns `"Etc/GMT+1"`, and
`format(as.POSIXct("2024-01-01 12:00:00", tz="UTC"), tz="Etc/GMT+1")` gives
`2024-01-01 11:00:00 -01`. A recorder whose metadata says UTC+1 would have its
times read two hours away from the truth. `cleanTZ("UTC-5")` is wrong by ten hours
in the same way. **CONFIRMED.**

Mitigating: `cleanTZ()` has no caller anywhere in `R/`; its only use in the repo
is `test-validationFunctions.R:116`, which asserts `cleanTZ("UTCa") ==
"Etc/GMTa"` — a string that is not a timezone, so the test pins the string
substitution without ever checking that the resulting zone means what the input
said. The bug is latent until the first real caller.

### 8. HHMM input is not range-checked — `R/convertTime.R:30-39`

Line 31 is a standing `# TODO: Validate hours and minutes`. The regex accepts any
one-to-four digit string, and the two halves are multiplied by 3600 and 60 with no
bound.

Failure scenario: `convert2seconds("2360", input.unit="HHMM")` returns `86400` —
24:00, a clock time that does not exist — and `convert2seconds("9999",
input.unit="HHMM")` returns `362340`, just over four days, for what is supposed to
be a time of day. `validateTimeInSeconds()` at the end of the function only
rejects negatives, so both pass. **CONFIRMED.**

### 9. `dolbear()` ignores its own validity range and returns negative chirp rates — `R/dolbear.R:47-55`

`dolbear.data()` carries `min` and `max` columns giving the temperature range each
regression was fitted over (`R/dolbear.R:92-107`), and nothing in `dolbear()` ever
reads them. Neither `n` nor `t` is validated.

Failure scenario: `dolbear(t=3)` returns `n = -6.85, -8.93, -13.99` chirps per
minute for the three *Oecanthus fultoni* populations — a negative rate of
chirping — with no warning, although all three regressions are documented as valid
only from 9 °C or 15 °C upward. `dolbear(t=-40)` returns `-341.7` chirps/minute.
In the other direction `dolbear(n=0)` returns `t = 3.88, 3.97, 4.70` °C, and
`dolbear(n=-50)` returns negative temperatures from a negative chirp rate.
**CONFIRMED.**

### 10. Audio size and sample arithmetic accept unphysical rates and depths — `R/audio_filesize.R:29`, `R/sDuration.R:23,53`

`duration` is validated through `convert2seconds()`, but `samp.rate`, `bit.depth`
and `channels` are multiplied raw, and `.resolveSampRate()` (`R/sDuration.R:66-77`)
only decides which of two arguments to use, never whether the value is possible.

Failure scenario: `audio_filesize(samp.rate=-44100, bit.depth=16, channels=1,
duration=1)` returns `-705600` bits; `audio_filesize(channels=-2)` returns a
negative size; `bit.depth=0` returns `0`. `sDuration(n=20, samp.rate=0)` returns
`Inf` seconds and `tSamples(time=-1, samp.rate=44100)` returns `-44100` samples.
**CONFIRMED.**

### 11. `.altitudePhase()` can never name a dawn, and its Day boundary does not match the event thresholds — `R/dayPhase.R:274-275`

The band table is `c(Day=0, Dusk.Civil=-6, Dusk.Naut=-12, Dusk.Astro=-18)`, so the
only names it can ever return are `Day`, `Dusk.Civil`, `Dusk.Naut`, `Dusk.Astro`
and `Night` — five of the ten in `.dayPhaseNames()`. The four `Dawn.*` names and
`Sunrise`/`Sunset` are unreachable from this path. Separately, the `Day` boundary
is 0°, whereas the event-driven classification puts `Day` at the `sunriseEnd`
threshold of −0.3° and `Sunrise`/`Sunset` between −0.833° and −0.3°, so the same
altitude is named differently depending on which path reached it.

Failure scenario: `dayPhase(time=as.POSIXct("2024-03-01 12:00:00", tz="UTC"),
duration=86400, lat=89.9, lon=0, tz="UTC")` names the phase `Dusk.Astro` for a
polar twilight that runs from 29 January to 5 March and is brightening toward
sunrise throughout — a consumer binning recordings by dawn versus dusk gets the
wrong bin for five weeks. Independently, an altitude of −0.2° is classified
`Dusk.Civil` here and `Day` by the event path. The naming-by-band rule is stated
in the function's own documentation, so the first half may be intended; the
boundary mismatch is not mentioned anywhere. **CONFIRMED** for the behaviour (band
table read directly; the polar example used the stubbed `suncalc`).

### 12. Rounding in `.humanUnits()` can emit a value that should have been promoted — `R/utils-units.R:98-108`

The unit is chosen from the unrounded value at line 98 and the value is rounded
afterwards at line 103, so a value just under a unit boundary rounds up to the
boundary while keeping the smaller unit. Separately, `findInterval()` returns 0 for
any negative value, so a negative count never gets a prefix.

Failure scenario: `humanBytes(999999.9)` returns `"1000 kB"` rather than `"1 MB"`;
`humanTime(3599.9999)` returns `"60 minutes"` rather than `"1 hour"`;
`humanTime(86399.9999)` returns `"24 hours"`. `humanBytes(-1500)` returns
`"-1500 bytes"` where `humanBytes(1500)` returns `"1.5 kB"`. **CONFIRMED.**

### 13. `.validateChoice()` accepts `NULL` and zero-length values — `R/validationFunctions.R:19-21`

`setdiff(NULL, choices)` has length 0, so the guard returns and the caller carries
on with a unit it cannot use. The error that eventually surfaces comes from deep
inside and names neither the argument nor the function.

Failure scenario: `convert2seconds(1, input.unit=NULL)` →
`Error: argument is of length zero` (from the `if` at `R/convertTime.R:27`, after
validation has passed); `convert2bytes(1, input.unit=NULL)` →
`Error: attempt to select less than one element in get1index`. Compare the
intended message, which `convert2seconds(1, "fortnights")` does produce:
`Unknown input.unit to convert2seconds: fortnights`. **CONFIRMED.**

### 14. The temperature converters bypass `.validateChoice()` — `R/convertTemperatures.R:22,27,32,36`

`convert2Kelvin()` and `convert2Celsius()` test the unit with a chain of bare
`if (input.unit == "C")` rather than the shared guard every other `convert2*()`
function uses, so a non-string unit fails on the comparison instead of being
reported.

Failure scenario: `convert2Kelvin(1, input.unit=NA)` →
`Error: missing value where TRUE/FALSE needed`;
`convert2Kelvin(1, input.unit=c("C","F"))` → `Error: the condition has length > 1`.
`convert2degrees(1, input.unit=NA)` by contrast gives the intended
`Unknown input.unit to convert2degrees: NA`. **CONFIRMED.**

### 15. `referencePressure()` offers no underwater reference — `R/referenceLevels.R:20-28`

The only reference pressure available is 20 µPa (returned as `2e-05` Pa, correct
for air), yet `soundSpeed()` ships values for `"sea water"` and `"freshwater"`,
so the package expects underwater work. The 1 µPa convention used underwater
cannot be obtained, and nothing documents that the value returned is air-only.
`referenceIntensity()` returns `1e-16` W cm⁻² = 1e-12 W m⁻², which is correct.

Failure scenario: anyone computing an underwater SPL from
`20*log10(p/referencePressure())` is 26 dB low with nothing to warn them.

Both functions are exported but have **no caller anywhere in `R/`**, and there is
no 10·log10 versus 20·log10 confusion to report because no dB calculation in the
package routes through either of them. **PLAUSIBLE** (a gap in the contract rather
than a reproduced wrong output).

### 16. The medium table mixes reference temperatures without saying so — `R/soundSpeed.R:89-90`

`"air"` is 343 m/s, which is dry air at 20 °C, while `"freshwater"` is 1430 m/s,
which is fresh water at roughly 6 °C — pure water at 20 °C is about 1482 m/s. The
documentation says only "Typical values", and no reference condition is given for
any entry.

Failure scenario: `wavelength(1000, speed=soundSpeed(medium="freshwater"))` gives
1.430 m where 20 °C water gives 1.482 m, a 3.6 % error, while the same call for air
is implicitly at a different temperature. **PLAUSIBLE.**

### 17. The enhancement factor uses π where Davis gives 3.14e-8 — `R/soundSpeed.R:135`

`ENH <- pi * 10^-8 * P + ...` substitutes π for the published coefficient
3.14 × 10⁻⁸ of Davis (1992). The difference is 5 × 10⁻⁴ relative in that
coefficient, about 1 × 10⁻⁶ m/s in the returned speed, so it changes no answer
anyone will notice — but it is not the constant the cited source gives, and a
reader checking the transcription against the paper will stop here.
**CONFIRMED** (read directly; magnitude computed).
