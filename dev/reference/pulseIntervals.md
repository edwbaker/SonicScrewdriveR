# Pulse intervals

Used to locate area of no pulses from the results of pulseDetection().

## Usage

``` r
pulseIntervals(pulses, nsd = 2)
```

## Arguments

- pulses:

  The result of a pulseDetection.

- nsd:

  The number of standard deviations each sid of the mean pulse interval
  to discard

## Value

A list of onset and offset times for pulses
