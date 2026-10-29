# Concatenate two or more Wave-like objects.

The `concat()` method is a more flexible version of the `bind()` method
from `tuneR` package, that allows specifying more advanced types of
concatenation. Setting `method` to "noClick" will remove any click
between Wave objects caused by sudden jumps in amplitude by applying
[`tuneR::prepComb()`](https://rdrr.io/pkg/tuneR/man/prepComb.html)
appropriately with default value of zero (this is only effective for the
left channel or stereo or multi-channel recordings).

## Usage

``` r
concat(object, ..., method = "bind")

# S4 method for class 'Wave'
concat(object, ..., method = "bind")

# S4 method for class 'WaveMC'
concat(object, ..., method = "bind")

# S4 method for class 'TaggedWave'
concat(object, ..., method = "bind")

# S4 method for class 'TaggedWaveMC'
concat(object, ..., method = "bind")
```

## Arguments

- object:

  A Wave like object.

- ...:

  Wave like objects to concatenate to object.

- method:

  One of "bind", "noClick". Default is "bind".

## Value

A concatenated Wave like object, with type of `object`.

## Details

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
The waves that were concatenated on are held in the arguments of the
record, each with its own tags and a digest of its samples.
