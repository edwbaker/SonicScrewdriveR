# Read Wildlife Acoustics metadata

Reads the wamd chunk of a wave file, the metadata Wildlife Acoustics
Song Meter recorders write: the model of the device, its serial number,
the firmware it was running, the prefix of the filename it wrote, the
time the recording started with the offset from UTC the device was set
to, the position its receiver had, the microphone fitted and the
identifications their software has since made. A file holding no wamd
chunk gives an empty result rather than an error.

## Usage

``` r
wamd(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with wamd metadata attached, a raw
  vector holding a wave file, or a wamd object

## Value

A wamd object, which is a named list of fields

## Details

The chunk names its fields by number rather than by name. The names used
here are those of the reference implementation published with the GUANO
specification; a field this package has no name for is reported under
its number, as `wamd:21`.

The embedded voice note, the program the device was running and the
state it was left in are binary rather than text and are passed over.
Their sizes are kept on the result as its "binary" attribute, so that a
file carrying them can be told from one that does not.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once.
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
reads the GUANO metadata that current Wildlife Acoustics firmware writes
in place of this format, and
[`parseFilename()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/parseFilename.md)
reads the filename conventions of these recorders.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- wamd("S4U09011_20210712_043000.wav")
m[["timestamp"]]
} # }
```
