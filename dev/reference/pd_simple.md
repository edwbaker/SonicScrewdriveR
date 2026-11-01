# Simplified pulse detection using Dietrich (2004)

Detects pulses in a Wave.

## Usage

``` r
pd_simple(
  wave,
  U = 120,
  gamma = 0.05,
  alpha = 1.4,
  scaling = 32,
  V = 480,
  psi = 1
)
```

## Arguments

- wave:

  A Wave object

- U:

  Window length

- gamma:

  Gamma

- alpha:

  Alpha

- scaling:

  Scaling

- V:

  V Window length

- psi:

  Psi

## Value

A list of input values plus the onset and offset times of pulses.

## Details

Onsets and offsets follow the convention described under
[`pulseDetection()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pulseDetection.md):
an onset is the first sample of a pulse and an offset the first sample
after it, so `offset - onset` is the length of the pulse in samples.

The detection thresholds a windowed energy, which rises over the half
window before a pulse begins and falls over the half window after it
ends, so each boundary is displaced outward by part of a half window and
a duration is overstated by part of a whole one. For a square pulse of
400 samples at the default `U` of 120 the duration comes back as 492
rather than 400. Two pulses separated by less than about `U` samples are
reported as one, because the energy does not fall below the threshold
between them. Both effects shrink with `U`, which is the time resolution
the detection is asked for.

ste() computes a window only for the samples that hold a whole one, so
the half window at each end of the wave is not analysed and a pulse
there is not reported. Where both windows are wanted, as they are here,
the longer of `U` and `V` sets how much of each end is unanalysed.
