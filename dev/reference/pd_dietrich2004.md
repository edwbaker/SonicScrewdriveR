# Pulse detection using Dietrich (2004)

Detects pulses in a Wave using the method described in Dietrich et al
(2004) <doi:10.1016/j.patcog.2004.04.004>.

## Usage

``` r
pd_dietrich2004(
  wave,
  U = 120,
  gamma = 0.05,
  alpha = 1.4,
  scaling = 32,
  V = 480,
  psi = 1,
  dt = 1,
  filter = TRUE,
  delta = 500,
  band = c(2000, 11000)
)
```

## Arguments

- wave:

  A Wave object

- U:

  Length in samples of the short window the pulse energy is computed
  over.

- gamma:

  Where `theta` sits between the smallest and the largest pulse energy,
  between 0 and 1.

- alpha:

  Multiplier giving the upper threshold `F_a` from the lower threshold
  `F_1`. Values above one put `F_a` above `F_1`.

- scaling:

  Divisor applied to the long window energy before it is added to
  `theta`.

- V:

  Length in samples of the long window the reference level is computed
  over. Expected to be longer than `U`.

- psi:

  Number of samples after the `F_1` crossing within which the energy
  must also cross `F_a` for a pulse to be reported. The paper names this
  parameter but never gives it a value.

- dt:

  The step the state machine takes through the wave, in samples. A
  boundary can only be reported on this grid. The paper names this
  parameter but never gives it a value either; see the details.

- filter:

  Whether to normalise and bandpass filter the wave first, which is the
  first step of the method. `FALSE` computes the energy from the wave as
  supplied.

- delta:

  Half the width of the passband, in Hz. The paper gives no value for
  this, only that the filter is a highly selective one.

- band:

  The frequency range the weighting `W` of equation 2 is defined over,
  as a pair in Hz, or `NULL` to search the whole spectrum for the
  loudest frequency. The default is the frequency range of cricket song,
  which section 1 of the paper gives as 2 to 11 kHz and which is what
  the paper's own `W` is a density over. A wave of some other animal
  needs its range given here.

## Value

A list of input values plus the onset and offset times of pulses

## Details

The short term energy is computed over a window of `U` samples (equation
5 of the paper) and over a longer window of `V` samples, which gives a
local reference level. Two thresholds are derived from the long window
energy (equation 7), `F_1 = theta + e_v/scaling` and
`F_a = theta + alpha*e_v/scaling`, where `theta` is the lower energy
bound of equation 8. As `alpha` is greater than one, `F_a` is the upper
of the two.

Algorithm 1 of the paper then walks the wave a sample at a time. A
candidate pulse opens at the sample where the energy first rises above
`F_1`, and is confirmed only if the energy also rises above `F_a` within
`psi` samples of that crossing. The sample reported is the `F_1`
crossing that opened the candidate, not the `F_a` crossing that
confirmed it. Where `F_a` is not reached in time the candidate is
abandoned and the next sample above `F_1` opens a new one, so a larger
`psi` lets a slow attack be reported from further back. Once a pulse has
been reported, no further pulse is reported until the energy falls below
`F_1` again.

The paper says only that a similar algorithm estimates the offsets. Here
the same scan is run over the wave in reverse, so an offset closes a run
above `F_1` that `F_a` confirmed within `psi` samples of its end. Both
boundaries are therefore measured at `F_1` and a pulse length is its
width at `F_1`.

Section 2 is two steps, and the segmentation above is the second. The
first, done here unless `filter` is `FALSE`, divides the wave by its
maximum norm (equation 1), finds the frequency `omega` carrying the most
energy, weighted by a density `W` over the frequencies the sound can
occupy (equation 2), and removes everything further than `delta` from it
(equations 3 and 4). This is what frees the energy of the sounds of the
environment before any of it is thresholded. The energies and thresholds
returned are then in the units of the normalised signal rather than of
the wave as supplied. Normalisation on its own changes no boundary, as
`theta`, `F_1` and `F_a` are all linear in the amplitude of the wave.

`band` is what keeps that first step useful, and it is worth setting for
the animal in hand. It gives the support of `W`, and so the range
equation 2 is allowed to choose from. Left to the whole spectrum, the
loudest frequency of a field recording is usually wind or handling noise
well below the song, and the filter then keeps that and discards the
song: on a wave of 5 kHz pulses over a 300 Hz drone, searching the whole
spectrum reported 593 pulses, one for each half cycle of the drone,
where the cricket range reported the 5 that are there.

Three of the method's parameters have no value anywhere in the paper,
and their defaults here are this package's rather than Dietrich's:
`psi`, which is named only in the prose and in Algorithm 1; `delta`,
described only as making a highly selective filter; and `dt`, the step
of Algorithm 1. The one candidate for `dt` is a row of Table 1 reading
`beta = U/4` among the segmentation parameters, under a symbol Algorithm
1 does not use and which is the sonogram overlap everywhere else in the
paper, so `U/4` is offered rather than assumed and the default is one
sample.

Onsets and offsets follow the convention described under
[`pulseDetection()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pulseDetection.md),
and the windowed energy displaces each boundary outward in the way
described under
[`pd_simple()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pd_simple.md).
The one point on which this function parts company with that convention,
and with
[`pd_simple()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pd_simple.md)
and `pd_threshold()`, is a sample whose energy is exactly equal to a
threshold. The paper's comparisons are not strict, so such a sample is
part of a pulse, and a silent or constant amplitude wave, whose every
energy is exactly equal to both thresholds, is reported as one pulse
spanning the analysed range. The other two detectors report no pulse in
it.
