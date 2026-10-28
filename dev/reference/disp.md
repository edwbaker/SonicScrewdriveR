# Read the display name of a wave file

Reads the DISP chunks of a wave file. The chunk holds one clipboard item
and the number of the clipboard format it is in; with a text format that
item is the name the file is to be shown under, which for a recording
out of a sound library is commonly its title or the place it was made.

## Usage

``` r
disp(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with DISP metadata attached, a raw
  vector holding a wave file, or a disp object

## Value

A disp object, which is a named list of fields: `format` naming the
clipboard format, and `text` where that format was a text one. A file
holding more than one DISP chunk gives `format.2` and `text.2` for the
second of them, and so on.

## Details

A chunk holding no more than the format number, which is how most
writers that mention the chunk at all leave it, gives nothing: there is
nothing in it to report. A chunk holding something in a format that is
not text gives the name of that format and no value, so that a caller
can tell the file carries an image rather than a title.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once.

## Examples

``` r
if (FALSE) { # \dontrun{
disp("recording.wav")
} # }
```
