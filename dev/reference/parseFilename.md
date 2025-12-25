# Parse a filename

Attempts to extract meaningful information from a filename, typically
the date and time a recording started.

## Usage

``` r
parseFilename(file, format = NULL, timezone = NULL)
```

## Arguments

- file:

  A filename (or list of filenames).

- format:

  Optionally force a given format (see Details). If NULL (default) an
  attempt is made to automatically detect the format for each file. If
  "match" and a list of filenames is given then an attempt will be made
  to find a format that matches all files. This may give incorrect
  results if the filename is ambiguous (see Details).

- timezone:

  Optionally set a timezone.

## Value

A list of file, type of match, datetime.  
  
It is possible to determine additional properties from some files, these
will be added to the list.

## Details

### Determining the format

It is sometimes impossible to accurately determine the format of a
filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers
it could be confused with a YYYYMMDD format. If a list of filenames is
given and the "match" format is specified then an effort will be made to
determine the most likely format that applies to all filenames.

### Supported formats

- **AudioMoth** - The newer format for AudioMoth devices consists of a
  standard YYYYMMDD_HHMMSS.wav format. Specifying 'AudioMoth' forces a
  call to the `audiomoth()` function from the `seewave` package (Sueur
  et al. 2008) .

- **AudioMoth HEX** - Older format for AudioMoth devices consisting of
  eight hexadecimal characters. Conversion is handled by a call to
  [`seewave::audiomoth()`](https://rdrr.io/pkg/seewave/man/audiomoth.html).

- **timestamp** - A standard date-time format. Uses the R standard
  origin of 1970-01-01 00:00:00 UTC.

- **Wildlife Acoustics SM2** - Can also be used for Wildlife Acoustics
  SM4 devices. Conversion is handled by a call to
  [`seewave::songmeter()`](https://rdrr.io/pkg/seewave/man/songmeter.html).

- **Wildlife Acoustics SM3** - Conversion is handled by a call to
  [`seewave::songmeter()`](https://rdrr.io/pkg/seewave/man/songmeter.html).

- **YYYYMMDD_HHMMSS** - A standard date-time format.

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
