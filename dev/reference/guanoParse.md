# Parse a block of GUANO metadata

Reads the text of a GUANO metadata block into a guano object. Fields the
specification defines are converted to the type it gives them, and any
other field, which includes every namespaced field, is kept as the
string it appears as in the file.

## Usage

``` r
guanoParse(text)
```

## Arguments

- text:

  A string holding a GUANO metadata block

## Value

A guano object, which is a named list of fields

## References

Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
<https://github.com/riggsd/guano-spec>

## Examples

``` r
guanoParse("GUANO|Version:1.0\nMake:Open Acoustic Devices\nSamplerate:48000")
#> GUANO|Version 1.0
#> Make          Open Acoustic Devices
#> Samplerate    48000
```
