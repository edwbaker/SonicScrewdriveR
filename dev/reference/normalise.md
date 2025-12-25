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
