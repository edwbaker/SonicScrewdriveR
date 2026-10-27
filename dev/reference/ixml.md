# Read the iXML metadata of a wave file

Reads the iXML chunk of a wave file, the convention by which field
recorders and the software that handles their files describe a take:
`PROJECT`, `SCENE`, `TAKE`, `TAPE` and `NOTE`, among many others that
particular recorders add.

## Usage

``` r
ixml(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with iXML metadata attached, a raw
  vector holding a wave file, or an ixml object

## Value

An ixml object, which is a named list of fields

## Details

Every element of the document that holds text rather than further
elements is read, named by its own tag, so that fields this package has
never heard of are read as readily as the ones it has. A tag used more
than once, as the entries of a track list use theirs, gives `TAG`,
`TAG.2` and so on. The document itself is kept on the result as its
"xml" attribute.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once.

## Examples

``` r
if (FALSE) { # \dontrun{
ixml("recording.wav")
} # }
```
