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

## Details

Each method reports a pulse as an onset and an offset. An onset is the
first sample of the pulse and an offset the first sample after it, so
the pulse occupies the half-open range from onset to offset and
`offset - onset` is its length in samples. A sample exactly equal to the
detection threshold is not part of a pulse.

`onsets[i]` and `offsets[i]` are always the two ends of the same pulse.
A pulse still in progress at the last analysed sample has an onset and
no offset, so there can be one more onset than offset, and in that case
the unmatched onset is the last. A pulse already in progress at the
first analysed sample is reported with its onset at that sample.

[`pd_dietrich2004()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pd_dietrich2004.md)
is the one exception to the rule about a sample equal to the threshold,
as the comparisons of the paper it implements are not strict ones. It is
described there.
