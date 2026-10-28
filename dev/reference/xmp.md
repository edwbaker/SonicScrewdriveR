# Read the XMP metadata of a wave file

Reads the `_PMX` chunk of a wave file, the chunk RIFF carries an XMP
packet in. XMP is the metadata format of Adobe's audio software, and a
file that has passed through an editor commonly holds the sample rate,
sample type, duration, tempo and key the editor worked out, beside the
title, artist, description and rights it was told.

## Usage

``` r
xmp(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with XMP metadata attached, a raw
  vector holding a wave file, or an xmp object

## Value

An xmp object, which is a named list of properties

## Details

Every property of the document is read, named as the document names it,
so that properties this package has never heard of are read as readily
as the ones it has. A property written as an attribute, as an element,
and as a list of alternatives all read the same way; where a list holds
one entry marked as the default language that entry is taken, and where
it holds several they are all given. A property named more than once
gives `name`, `name.2` and so on. The packet itself is kept on the
result as its "xml" attribute.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once.

## Examples

``` r
if (FALSE) { # \dontrun{
xmp("recording.wav")
} # }
```
