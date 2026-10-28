# Read the INFO metadata of a wave file

Reads the fields of the RIFF lists of a wave file. The list of
information is where a recorder, an editor or an archive writes what it
has to say about a recording in no particular format: `ICMT` holds a
comment, which is where AudioMoth firmware records the settings a
recording was made with, `INAM` a name, `IART` an artist and `ICRD` a
date. A file holding no list gives an empty result rather than an error.

## Usage

``` r
wavInfo(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with INFO metadata attached, a raw
  vector holding a wave file, or a wavInfo object

## Value

A wavInfo object, which is a named list of fields

## Details

Fields of the list of information are named by their own four-character
identifiers. A file may hold other lists, most often `adtl`, which
carries the labels of cue points rather than anything about the
recording as a whole; the fields of those are named by their list as
well, as in `adtl/labl`. A label read here is the text alone:
[`wavCues()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavCues.md)
reads the same labels with the positions in the recording they name.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once, and
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md),
[`bext()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bext.md),
[`wamd()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wamd.md)
and
[`ixml()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/ixml.md)
read the other formats on their own.

## Examples

``` r
f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
wavInfo(f)
#> ICMT Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at medium gain setting while battery state was 4.1V and temperature was 5.8C.
#> IART AudioMoth 240435055C7B17DB
```
