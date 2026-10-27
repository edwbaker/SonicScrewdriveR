# Read Broadcast Wave metadata

Reads the bext chunk of a wave file, the Broadcast Wave metadata written
by recorders and editing software built for broadcast and by the
archives that take their files. The description is a free run of text,
and a sound library commonly puts the whole of its record for a
recording into it, as in "Acinipe calabra; Studio recording; Italy,
Sicily, Baucina (PA); Female song". A file holding no bext chunk gives
an empty result rather than an error.

## Usage

``` r
bext(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with bext metadata attached, a raw
  vector holding a wave file, or a bext object

## Value

A bext object, which is a named list of fields

## Details

Fields are named as the specification names them. The date and time are
also reported together as `datetime`; the file says nothing about which
timezone they were written in, so they are read in the timezone of the
session. A time written in no layout the specification allows leaves the
date at midnight, which is what a file that left the time empty already
gave, rather than taking the date down with it.

## References

European Broadcasting Union (2011). Specification of the Broadcast Wave
Format: a format for audio data files in broadcasting. EBU Tech 3285.
<https://tech.ebu.ch/publications/tech3285>

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once, and
[`ixml()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/ixml.md)
reads the iXML chunk that a recorder writing this format often writes
beside it.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- bext("recording.wav")
m[["Description"]]
} # }
```
