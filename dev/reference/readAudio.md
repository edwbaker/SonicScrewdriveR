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
