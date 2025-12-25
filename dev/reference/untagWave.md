# Untag a TaggedWave or TaggedWaveMC object

This function takes a TaggedWave/TaggedWaveMC object (or a list of such
objects) and returns a corresponding Wave/WaveMC object (or list of such
objects).

## Usage

``` r
untagWave(w)
```

## Arguments

- w:

  A TaggedWave or TaggedWaveMC object (or list of such objects).

## Value

A Wave or WaveMC object.

## Examples

``` r
if (FALSE) { # \dontrun{
w <- noise("white")
tw <- tagWave(w)
w2 <- untagWave(tw)
} # }
```
