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

A list of input values plus the onset and offset times of pulses
