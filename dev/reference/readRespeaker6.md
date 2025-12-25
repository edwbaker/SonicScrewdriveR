# Read a file from Seeed Studio Respeaker 6 mic array

The Seeed Studio Respeaker-6 when used as described in the documentation
saves an eight channel audio file with channels 7 and 8 not containing
input audio. This function reads such a file and saves it as a six
channel file.

## Usage

``` r
readRespeaker6(filename, from = 1, to = Inf, units = "samples", header = FALSE)
```

## Arguments

- filename:

  file to read.

- from:

  Where to start reading the wave in units.

- to:

  Where to stop reading the wave in units.

- units:

  Units in which from and to is given, the default is "samples", but can
  be set to time intervals such as "seconds".

- header:

  If TRUE, just header information of the Wave file are returned,
  otherwise (the default) the whole Wave object.

## Value

A WaveMC object.
