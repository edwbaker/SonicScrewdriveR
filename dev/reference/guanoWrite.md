# Write GUANO metadata to a WAV file

Replaces the GUANO metadata of a WAV file, adding a guan chunk if the
file has none. Only the metadata chunk and the sizes the file records
are changed: those in the file header, or in the ds64 chunk of an RF64
or BW64 file, the form a recording over 4 GiB is written in. The audio
data and every other chunk are left as they were.

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

## Details

The recording is copied across a buffer at a time rather than read into
memory, so changing one field of the metadata of a file of several
gigabytes takes no more memory than changing it in a short one.

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
