# Pulse detection

Detects pulses in a Wave. Stereo and multi-channel waves are analysed
one channel at a time, giving one result per channel.

## Usage

``` r
pulseDetection(wave, method = "simple", ...)
```

## Arguments

- wave:

  A Wave object containing pulses

- method:

  Which method to use for pulse detection

- ...:

  Other arguments to pass to pulse detection function

## Value

The output of the pulse detection method used.
