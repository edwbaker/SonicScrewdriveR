# Attach metadata to a Wave object

A wave keeps the metadata of its file in one place, which
[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads back, and where that place is depends on what it is. A TaggedWave
has a `metadata` slot made for this, so the reading goes in there, under
the name "audioMetadata" and beside anything else the caller has
recorded in that slot. A plain Wave has nowhere of its own, so an
attribute of that same name is used instead.

## Usage

``` r
audioMetadata(x) <- value
```

## Arguments

- x:

  A Wave, WaveMC, TaggedWave or TaggedWaveMC object

- value:

  An audioMetadata object, from
  [`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md),
  or NULL to remove what is attached

## Value

The object with the metadata attached

## Details

`guano<-` writes into the same place, so a wave does not end up carrying
one record of its file in one place and another somewhere else.

Writing the wave to a file does not carry the metadata with it: use
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md)
on the written file, or pass the metadata to
[`audiomothCompress()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothCompress.md).

## See also

`guano<-` attaches GUANO metadata on its own, in a form
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md)
can write back to a file.

## Examples

``` r
if (FALSE) { # \dontrun{
audioMetadata(wave) <- readMetadata("recording.wav")
readMetadata(wave)$fields$datetime
} # }
```
