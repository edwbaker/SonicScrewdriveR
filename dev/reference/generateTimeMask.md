# Generate time masked Wave-like objects

Given a `Wave`-like object (or a list of `Wave`-like objects), generate
new `Wave`-like objects with time masking.

## Usage

``` r
generateTimeMask(wave, method = "squarewave", dutyCycle = 0.95, n.periods = 10)
```

## Arguments

- wave:

  A `Wave`-like object (or a list of `Wave`-like objects).

- method:

  The method to use for time masking (one of "squarewave", "random).

- dutyCycle:

  The duty cycle of the output. A value of 0.95 means that 5% of the
  time is masked.

- n.periods:

  The number of waves to generate in the squarewave method.
