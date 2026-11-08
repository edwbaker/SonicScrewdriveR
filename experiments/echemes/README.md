# Echeme segmentation: an experiment

Nothing here is part of `sonicscrewdriver`. The detector is sourced over a
loaded copy of the package so that it can use the package's own `pulseStats()`
and validation helpers; it is deliberately not in `R/`, is not exported, and
`experiments/` is in `.Rbuildignore`.

The question is whether the pulses `pd_dietrich2004()` reports in a cricket
recording can be grouped into echemes, the level of the song above the pulse.

## The hard part is deciding whether there are any

Placing boundaries is easy: split the pulse distances into a short class and a
long class, and a new echeme starts after every long distance. The difficulty
is that a song which runs on without grouping — the trill of many *Oecanthus* —
has no echemes at all, and splitting at the longest gap always invents some.
Over 27 recordings of *Oecanthus pictus* an unconditional split gave between 1
and 468 pulses per echeme for the one species, a spread of 8.9 octaves.

Two conditions therefore have to hold before any echeme is reported:

- the median group must hold at least `min.pulses` pulses (default 2) — a group
  of one pulse is a pulse, not a group of them; and
- `separation` must be at least `min.separation` (default 0.8) — the share of
  the variance in the log distances that the division accounts for, so that the
  distances really do fall into two classes rather than one.

The division itself is Otsu's method in one dimension on `log1p(distance)`: the
two kinds of silence differ by a factor, not by a number of samples, and `log1p`
rather than `log` keeps a touching pair of pulses in the short class instead of
dropping it for having no logarithm.

Where either condition fails, no echeme is reported and the onsets and offsets
come back empty. That is an answer about the song, not a failure, and the
diagnostics are still returned so the reason can be read off.

## What the refusal costs and buys

Measured over 41 Gryllidae recordings that pass a spectral gate (peak
concentration >= 0.95 and SNR >= 15 dB), from `acc.R`:

```
  before: 41/41 recordings grouped, worst within species spread 8.87 oct
  after:   8/41 recordings grouped, worst within species spread 0.81 oct
```

It refuses four fifths of the recordings, including all 27 *O. pictus* trills,
which is the correct answer for a trill. Against catalogued ranges for the
species that survive:

- echeme rate: 3/3 within the catalogued range
- syllables per echeme: 2/4 within range, median 0.03 octaves from the nearest
  range edge (the two misses are *Eumodicogryllus bordigalensis*, 15 against
  11–13, and *Oecanthus pellucens*, 41.5 against 15–40)

The 0.81 octaves that remain are one species, and they are temperature, not
error:

```
    id degC nEch echHz nsyl separation
  9952 21.0  119  2.25    4      0.976
 11120 21.0  150  2.67    4      0.955
 11119 23.0  229  4.02    4      0.981
  9707 28.5  254  4.77    7      0.809
```

*Gryllus campestris* gives 4 syllables per echeme at 21–23 °C and 7 at 28.5 °C,
with the echeme rate climbing 2.25 to 4.77 Hz over the same span, about
0.13 octaves per °C.

For scale, at the pulse level the same pipeline is 4–9 times finer than the
width of the catalogued range it is being compared with (0.07–0.15 octaves
against 0.62). At the echeme level it is only 0.5–1.0 times the target width for
echeme rate and 0.3–1.0 for syllables per echeme, so echeme "hits" are flattered
by how wide the catalogued ranges are. Pulse rate is recovered; echeme structure
is plausible but not yet demonstrated to that standard.

## Files

| File | What it does |
| --- | --- |
| `echemeDetector.R` | `echemes()` and `.echemeSplit()` — the detector |
| `test-echemeDetector.R` | 44 assertions over synthetic songs |
| `runTests.R` | loads the package, sources the detector, runs the tests |
| `smoke.R` | the same cases printed rather than asserted |
| `proto.R` | runs the pulse detector over the gated recordings, writes `proto.rds` |
| `rule.R` | compares candidate rules against `proto.rds` — how the two defaults were chosen |
| `validate.R` | runs the detector as written over the same recordings, writes `validate.rds` |
| `acc.R` | `validate.rds` against the catalogued ranges |

Run every script from the repository root:

```
Rscript experiments/echemes/runTests.R
Rscript experiments/echemes/acc.R
```

`runTests.R`, `smoke.R`, `rule.R` and `acc.R` need no audio. `proto.R` and
`validate.R` read 41 recordings and take several minutes.

## Data

`gryl_fetch.rds` lists 63 Gryllidae recordings from audioBLAST, all of them
bio.acousti.ca material under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Its `share` column
points at the audioblast data share rather than at `files.audioblast.org`, and
the scripts prefer that column; the share holds all 63.

`gryl_temp_measured2.rds` holds the per-recording measurements and the
`temperature_start` the audioBLAST `details` module records in °C.
`gryl_traits.rds` holds the catalogued trait ranges the measurements are
compared against. `proto.rds` and `validate.rds` are outputs, committed so that
`rule.R` and `acc.R` run without the audio.
