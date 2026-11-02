# Pulse intervals

Used to locate areas of no pulses in the result of a pulseDetection().

## Usage

``` r
pulseIntervals(pulses, nsd = 2)
```

## Arguments

- pulses:

  The result of a pulseDetection(), holding onsets and offsets.

- nsd:

  The number of standard deviations above the mean gap that a gap must
  be to be reported as an area of no pulses.

## Value

A list of the onset and offset of each area of no pulses: the first
sample that is not part of a pulse, and the first sample of the next
pulse.

## Details

A gap runs from the offset of one pulse to the onset of the next,
following the convention described under
[`pulseDetection()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pulseDetection.md),
so it holds exactly the samples that are not part of a pulse and its
length is `onsets[i + 1] - offsets[i]`. n pulses have n - 1 gaps between
them, and neither no pulses nor one pulse has any, so nothing is
reported for fewer than two. A pulse the recording ends part way through
has an onset and no offset, and no gap follows it.

A gap is reported if it is more than `nsd` standard deviations above the
mean of the other gaps. The test is one-sided, as an unusually short gap
is not an area of no pulses. Each gap is left out of the mean and the
standard deviation it is compared with, so that a long gap does not lift
the cut-off it has to clear. Where the other gaps are all the same
length their standard deviation is zero, and any gap longer than them is
reported. At least three gaps, which is four pulses, are needed, and a
call with fewer says so.
