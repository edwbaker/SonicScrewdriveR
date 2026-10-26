# Write GUANO metadata to a WAV file

Replaces the GUANO metadata of a WAV file, adding a guan chunk if the
file has none. Only the metadata chunk and the sizes in the file header
are changed, so the audio data and every other chunk are left as they
were.

## Usage

``` r
guanoWrite(x, file, output = NULL)
```

## Arguments

- x:

  A guano object, from
  [`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
  or
  [`guanoCreate()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoCreate.md)

- file:

  Path to the WAV file to read

- output:

  Path to write to. If NULL (the default) `file` is rewritten in place.

## Value

The path to the written file (invisibly)

## References

Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
<https://github.com/riggsd/guano-spec>

## Examples

``` r
if (FALSE) { # \dontrun{
m <- guano("20201103_112430.WAV")
m[["Species Manual ID"]] <- "Pipistrellus pipistrellus"
guanoWrite(m, "20201103_112430.WAV")
} # }
```
