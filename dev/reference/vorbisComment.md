# Read Vorbis comments

Reads the Vorbis comments of a FLAC or Ogg file, the metadata those
formats carry: `TITLE`, `ARTIST`, `DATE`, `LOCATION` and `COMMENT` among
the names in common use, and whatever else the software that wrote the
file chose to add. A file holding no comments gives an empty result
rather than an error.

## Usage

``` r
vorbisComment(x)
```

## Arguments

- x:

  Path to a FLAC or Ogg file, a Wave object with Vorbis comments
  attached, a raw vector holding a file, or a vorbisComment object

## Value

A vorbisComment object, which is a named list of fields

## Details

Fields are named as the file names them, which is a convention rather
than a specification: the format reserves no names and a writer may use
any. The vendor string, which names the software that encoded the audio,
is reported as `vendor`.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- vorbisComment("recording.flac")
m[["TITLE"]]
} # }
```
