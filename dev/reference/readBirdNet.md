# Read output files from BirdNet Analyser

Reads a single file, or directory of files, output by BirdNet Analyser.

## Usage

``` r
readBirdNet(file, filename_parsing = "none")
```

## Arguments

- file:

  Filename or directory

- filename_parsing:

  Allows for filename parsing, accepted values are one of none,
  audiomoth, timestamp.

## Value

A data frame. A directory holding no results file this reads gives one
of no rows, with a warning, rather than NULL.

## Details

BirdNET writes its results in one of several layouts, which hold
different columns and are not all separated by the same character, and
which have changed across its releases. The layout is worked out from
the file's own header rather than assumed, so a Raven selection table, a
`csv`, an `rtable` and a Kaleidoscope file all read, and the begin and
end of each detection are taken from whichever columns that layout keeps
them in. A Kaleidoscope file gives how long a detection lasts rather
than when it ends, and an end is worked out from it.

A file in none of those layouts is refused by name, with its first line
and the layouts that are known, where assigning to a column the file
does not have reported only `replacement has 0 rows, data has 1`. In a
directory such a file is passed over with a warning instead, since a
results directory commonly holds files that are not results.

A `filename_parsing` the name cannot be read by is an error naming the
file and the convention asked for. Reading an AudioMoth name as a
timestamp gave every detection a time of NA and said only
`NAs introduced by coercion`.

## See also

[`readAudacityLabels()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudacityLabels.md)
reads the Audacity labels BirdNET also writes, which carry no header.
