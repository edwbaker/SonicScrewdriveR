# Read GUANO metadata

Reads the GUANO metadata of a WAV file, or of a Wave object that carries
it. GUANO is the metadata format used by many bioacoustic recorders,
including AudioMoth devices running current firmware. A file holding no
GUANO metadata gives an empty result rather than an error.

## Usage

``` r
guano(x)
```

## Arguments

- x:

  Path to a WAV file, a Wave object with GUANO metadata attached, or a
  raw vector holding a whole WAV file

## Value

A guano object, which is a named list of fields

## References

Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
<https://github.com/riggsd/guano-spec>

## Examples

``` r
if (FALSE) { # \dontrun{
m <- guano("20201103_112430.WAV")
m[["Loc Position"]]
} # }
```
