# Parse a filename

Attempts to extract meaningful information from a filename, typically
the date and time a recording started.

## Usage

``` r
parseFilename(file, format = NULL, timezone = "UTC")
```

## Arguments

- file:

  A filename, or several filenames as either a list or a character
  vector (the shape returned by
  [`list.files()`](https://rdrr.io/r/base/list.files.html)).

- format:

  Optionally force a given format (see Details). If NULL (default) an
  attempt is made to automatically detect the format for each file. If
  "match" and a list of filenames is given then an attempt will be made
  to find a format that matches all files. This may give incorrect
  results if the filename is ambiguous (see Details).

- timezone:

  The timezone the clock of the recorder was set to, defaults to "UTC"
  (see Details).

## Value

A list of file, type of match, datetime. If a list of filenames is given
then a list of such lists is returned.  
  
It is possible to determine additional properties from some files, these
will be added to the list. Which properties those are depends on the
format: a Wildlife Acoustics name gives a model, a prefix, a microphone
and a deployment, a TSA name gives a collection, a taxon, an accession
number and a hash, and a reading the name left in doubt gives
`ambiguous` and `alternatives`. A format carrying no time, which TSA
names do not, reports `datetime` as NA rather than leaving it out.

## Details

### Determining the format

It is sometimes impossible to accurately determine the format of a
filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers
it could be confused with a YYYYMMDD format. If a list of filenames is
given and the "match" format is specified then an effort will be made to
determine the most likely format that applies to all filenames.

A reading that could not be settled from the name says so: `ambiguous`
is TRUE and `alternatives` names every format the filename could be in,
the one that was picked included. Neither field is present where the
format was not in doubt. A name of nothing but digits is the case that
matters, because a collection that numbers its records gives names of
exactly that shape: eight digits are read as a date and ten as a count
of seconds, and both read just as well as a record number.
[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
therefore takes no date from such a name, as a confident wrong date is
worse than none, though it is reported here for a caller that knows what
its own files are named by.

Asking for a format by name settles it: a format given in `format` is
not a guess and carries no alternatives. Finding one that fits every
name in a list, with `format = "match"`, does not settle it, since a
list of record numbers agrees with itself exactly as readily as a list
of dates does.

### Timezones

Most supported formats write a wall clock (e.g. 20200609_191730) into
the filename, with no indication of the timezone the clock of the
recorder was set to. The `timezone` argument states which timezone that
wall clock belongs to, and defaults to UTC, which is what AudioMoth
devices use unless they are configured otherwise. The datetime returned
is therefore the same instant whatever timezone the machine running the
analysis is set to.

The 'AudioMoth HEX' and 'timestamp' formats are exceptions: these encode
an instant rather than a wall clock, so for them `timezone` only sets
the timezone the datetime is displayed in.

### Supported formats

- **AudioMoth** - The newer format for AudioMoth devices consists of a
  standard YYYYMMDD_HHMMSS.wav format. An eight character hexadecimal
  filename given this format is treated as 'AudioMoth HEX'.

- **AudioMoth HEX** - Older format for AudioMoth devices consisting of
  eight hexadecimal characters giving the number of seconds since
  1970-01-01 00:00:00 UTC. Conversion is handled by a call to the
  `audiomoth()` function from the `seewave` package (Sueur et al. 2008)
  .

- **timestamp** - A standard date-time format. Uses the R standard
  origin of 1970-01-01 00:00:00 UTC.

- **TSA** - The convention of the TSA corpus and the collections
  distributed with it, as
  `TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav`: a collection,
  the taxon or common name of the animal recorded with the accession
  number the collection gave it, and a hash of the file. Gives
  `collection`, `taxon`, `accession` and `hash`, and no datetime, as
  these names carry no time. Much of that corpus holds no embedded
  metadata at all, so for those recordings the name is the only record
  of what was recorded.

  A `taxon` is read only where the name carries a scientific name, which
  is a capitalised genus and a lowercase specific epithet joined by an
  underscore or a hyphen. Part of the corpus is named by a German common
  name and a German word for the kind of call instead; no taxon is read
  from those, as reporting their first words as a genus and a species
  would invent one. A subspecies cannot be told from an accession number
  that happens to be a bare word, so the first two parts of a scientific
  name are taken as the name and the rest as the accession number.

- **Wildlife Acoustics SM2** - Can also be used for Wildlife Acoustics
  SM4 devices. The filename is split by a call to
  [`seewave::songmeter()`](https://rdrr.io/pkg/seewave/man/songmeter.html).

- **Wildlife Acoustics SM3** - The filename is split by a call to
  [`seewave::songmeter()`](https://rdrr.io/pkg/seewave/man/songmeter.html).

- **YYYYMMDD_HHMMSS** - A standard date-time format.

- **YYYYMMDD** - A standard date format, the time is taken to be
  midnight.

## References

Sueur J, Aubin T, Simonis C (2008). “Seewave, a free modular tool for
sound analysis and synthesis.” *Bioacoustics*, **18**(2), 213–226.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads a file's own embedded metadata and puts what its name says beside
it.

## Examples

``` r
parseFilename("5E90A4D4.wav")
#> $filename
#> [1] "5E90A4D4.wav"
#> 
#> $match
#> [1] "AudioMoth HEX"
#> 
#> $datetime
#> [1] "2020-04-10 16:54:44 UTC"
#> 
parseFilename("TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")$taxon
#> [1] "Porzana parva"

# Eight digits could be a date or a record number, and the reading says so.
parseFilename("20240220.wav")$alternatives
#> [1] "YYYYMMDD"      "AudioMoth HEX" "record number"
```
