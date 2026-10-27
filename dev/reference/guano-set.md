# Attach GUANO metadata to a Wave object

Wave objects have no place to keep file metadata, so it is kept in the
one store a wave carries for it, which
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
reads back: the metadata slot of a TaggedWave, or an attribute of a
plain Wave. GUANO is recorded there alongside any other format the file
carried, so setting it leaves the rest of what the wave says about its
file alone, and
[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads all of it together.

## Usage

``` r
guano(x) <- value
```

## Arguments

- x:

  A Wave, WaveMC, TaggedWave or TaggedWaveMC object

- value:

  A guano object, or NULL to remove the metadata

## Value

The object with the metadata attached

## Details

Writing the wave to a file does not carry the metadata with it: use
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md)
on the written file, or pass the metadata to
[`audiomothCompress()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothCompress.md).

## See also

`audioMetadata<-` attaches a whole reading, of every format at once.

## Examples

``` r
if (FALSE) { # \dontrun{
guano(wave) <- guanoCreate(Make="Open Acoustic Devices", Model="AudioMoth")
} # }
```
