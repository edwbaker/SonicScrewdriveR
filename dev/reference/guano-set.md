# Attach GUANO metadata to a Wave object

Wave objects have no place to keep file metadata, so GUANO metadata read
from a file is kept as an attribute, which
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
reads back. Writing the wave to a file does not carry the metadata with
it: use
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md)
on the written file, or pass the metadata to
[`audiomothCompress()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothCompress.md).

## Usage

``` r
guano(x) <- value
```

## Arguments

- x:

  A Wave or WaveMC object

- value:

  A guano object, or NULL to remove the metadata

## Value

The object with the metadata attached

## Examples

``` r
if (FALSE) { # \dontrun{
guano(wave) <- guanoCreate(Make="Open Acoustic Devices", Model="AudioMoth")
} # }
```
