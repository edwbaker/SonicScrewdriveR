# Read an audio file

This file is used to read an audio file and return a Wave object, it is
an abstraction function for various specific audio reading functions. If
no existing method can be identified an attempt is made to use the av
package to read the audio.

## Usage

``` r
readAudio(file, mime = "auto", from = 0, to = Inf, units = "seconds")
```

## Arguments

- file:

  File to read

- mime:

  MIME type of file to read, or "auto". Supported types are
  "audio/x-wav" and "audio/mpeg" (MP3)

- from:

  Start point in file to return

- to:

  End point in file to return

- units:

  One of "samples", "seconds", "minutes", "hours". Default is "seconds".

## Value

A Wave object

## Details

Files read through the av package (which is everything that is not WAVE
or MP3, e.g. FLAC) are returned at the bit depth the file was stored at,
so that the same audio compares equal however it was read. The av
package does not report a bit depth of 24, and returns 24bit audio as
32bit, so the samples of a 24bit file are 256 times those
[`tuneR::readWave()`](https://rdrr.io/pkg/tuneR/man/readWave.html) would
give for the same audio stored as WAVE. Audio stored as floating point
is returned as 32bit.
