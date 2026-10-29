# Normalise a Wave object

Similar to normalize() from the tuneR package but automatically
identifies the unit parameter.

## Usage

``` r
normalise(wave, unit = NULL, ...)
```

## Arguments

- wave:

  Wave or WaveMC object

- unit:

  If not null behaves as in normalize() from tuneR, if null the unit is
  automatically identified.

- ...:

  Additional arguments passed to normalize() from tuneR

## Value

Normalised Wave or WaveMC object

## Details

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
The unit that was used is recorded, rather than the `NULL` that may have
been asked for.
