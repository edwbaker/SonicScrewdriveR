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
will be added to the list.

## Details

### Determining the format

It is sometimes impossible to accurately determine the format of a
filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers
it could be confused with a YYYYMMDD format. If a list of filenames is
given and the "match" format is specified then an effort will be made to
determine the most likely format that applies to all filenames.

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
```
