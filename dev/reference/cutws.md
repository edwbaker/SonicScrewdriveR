# Cut wave by samples

Extract a section of a Wave or WaveMC object based on sample positions.
This function will automatically detect if a Wave object is stereo, and
returns the same class of object it was given.

## Usage

``` r
cutws(wave, from = 1, to = Inf, plot = FALSE)
```

## Arguments

- wave:

  A Wave or WaveMC object

- from:

  First sample to return

- to:

  Last sample to return. A value beyond the end of the wave is cut to
  the final sample.

- plot:

  If TRUE shows the cut region within the original waveform

## Value

A Wave or WaveMC object

## Details

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
The samples that were cut to are recorded, rather than the ones that
were asked for, which may have run beyond the end of the wave.

## Examples

``` r
if (FALSE) { # \dontrun{
cutws(sheep, 1, 20)
cutws(sheep, 1, 20, plot=TRUE)
} # }
```
