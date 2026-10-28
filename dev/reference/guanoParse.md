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

## Details

A `Timestamp` written in a form the specification does not allow, but
whose meaning is plain, is read, with a message naming what departs from
it: a space between the date and the time where the specification has a
T, as a Wildlife Acoustics Song Meter writes, an offset hour of one
digit, or an offset with no colon. It is written back in the form the
specification gives, keeping its offset from UTC.

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
